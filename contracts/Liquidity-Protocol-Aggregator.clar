(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-dex (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-slippage-too-high (err u103))
(define-constant err-route-not-found (err u104))
(define-constant err-execution-failed (err u105))

(define-map dex-registry
  { dex-id: uint }
  {
    name: (string-ascii 32),
    contract-address: principal,
    active: bool,
    fee-basis-points: uint
  }
)

(define-map token-pairs
  { token-in: principal, token-out: principal }
  { exists: bool }
)

(define-map dex-prices
  { dex-id: uint, token-in: principal, token-out: principal }
  {
    price: uint,
    liquidity: uint,
    last-update: uint
  }
)

(define-map user-trade-stats
  { user: principal }
  {
    total-trades: uint,
    total-volume: uint,
    last-trade-block: uint
  }
)

(define-data-var next-dex-id uint u1)
(define-data-var protocol-fee-basis-points uint u10)
(define-data-var max-slippage-basis-points uint u500)

(define-read-only (get-dex-info (dex-id uint))
  (map-get? dex-registry { dex-id: dex-id })
)

(define-read-only (get-price (dex-id uint) (token-in principal) (token-out principal))
  (map-get? dex-prices { dex-id: dex-id, token-in: token-in, token-out: token-out })
)

(define-read-only (get-user-stats (user principal))
  (default-to
    { total-trades: u0, total-volume: u0, last-trade-block: u0 }
    (map-get? user-trade-stats { user: user })
  )
)

(define-read-only (calculate-output (amount-in uint) (price uint))
  (/ (* amount-in price) u1000000)
)

(define-read-only (calculate-fee (amount uint) (fee-bp uint))
  (/ (* amount fee-bp) u10000)
)

(define-read-only (find-best-price (token-in principal) (token-out principal) (amount-in uint))
  (let
    (
      (price-1 (get-price u1 token-in token-out))
      (price-2 (get-price u2 token-in token-out))
      (price-3 (get-price u3 token-in token-out))
      (result (find-best-price-helper price-1 price-2 price-3 amount-in))
    )
    (asserts! (> (get best-output result) u0) err-route-not-found)
    (ok result)
  )
)

(define-private (find-best-price-helper 
  (price-1 (optional { price: uint, liquidity: uint, last-update: uint }))
  (price-2 (optional { price: uint, liquidity: uint, last-update: uint }))
  (price-3 (optional { price: uint, liquidity: uint, last-update: uint }))
  (amount-in uint))
  (let
    (
      (output-1 (match price-1 p (calculate-output amount-in (get price p)) u0))
      (output-2 (match price-2 p (calculate-output amount-in (get price p)) u0))
      (output-3 (match price-3 p (calculate-output amount-in (get price p)) u0))
    )
    {
      best-dex-id: (if (and (>= output-1 output-2) (>= output-1 output-3))
                      u1
                      (if (>= output-2 output-3) u2 u3)),
      best-output: (if (and (>= output-1 output-2) (>= output-1 output-3))
                      output-1
                      (if (>= output-2 output-3) output-2 output-3))
    }
  )
)

(define-read-only (get-protocol-fee)
  (var-get protocol-fee-basis-points)
)

(define-read-only (get-max-slippage)
  (var-get max-slippage-basis-points)
)

(define-public (register-dex (name (string-ascii 32)) (contract-addr principal) (fee-bp uint))
  (let
    (
      (dex-id (var-get next-dex-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set dex-registry
      { dex-id: dex-id }
      {
        name: name,
        contract-address: contract-addr,
        active: true,
        fee-basis-points: fee-bp
      }
    )
    (var-set next-dex-id (+ dex-id u1))
    (ok dex-id)
  )
)

(define-public (update-price (dex-id uint) (token-in principal) (token-out principal) (price uint) (liquidity uint))
  (begin
    (asserts! (is-some (get-dex-info dex-id)) err-invalid-dex)
    (map-set dex-prices
      { dex-id: dex-id, token-in: token-in, token-out: token-out }
      {
        price: price,
        liquidity: liquidity,
        last-update: stacks-block-height
      }
    )
    (map-set token-pairs
      { token-in: token-in, token-out: token-out }
      { exists: true }
    )
    (ok true)
  )
)

(define-public (execute-swap 
  (token-in principal)
  (token-out principal)
  (amount-in uint)
  (min-amount-out uint))
  (let
    (
      (best-route-result (try! (find-best-price token-in token-out amount-in)))
      (best-dex-id (get best-dex-id best-route-result))
      (expected-output (get best-output best-route-result))
      (protocol-fee (calculate-fee expected-output (var-get protocol-fee-basis-points)))
      (final-output (- expected-output protocol-fee))
      (user-stats (get-user-stats tx-sender))
    )
    (asserts! (> amount-in u0) err-invalid-amount)
    (asserts! (>= final-output min-amount-out) err-slippage-too-high)
    (map-set user-trade-stats
      { user: tx-sender }
      {
        total-trades: (+ (get total-trades user-stats) u1),
        total-volume: (+ (get total-volume user-stats) amount-in),
        last-trade-block: stacks-block-height
      }
    )
    (ok {
      dex-id: best-dex-id,
      amount-out: final-output,
      protocol-fee: protocol-fee
    })
  )
)

(define-public (set-protocol-fee (new-fee-bp uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-bp u100) err-invalid-amount)
    (var-set protocol-fee-basis-points new-fee-bp)
    (ok true)
  )
)

(define-public (set-max-slippage (new-slippage-bp uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-slippage-bp u1000) err-invalid-amount)
    (var-set max-slippage-basis-points new-slippage-bp)
    (ok true)
  )
)

(define-public (toggle-dex (dex-id uint) (active bool))
  (let
    (
      (dex-info (unwrap! (get-dex-info dex-id) err-invalid-dex))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set dex-registry
      { dex-id: dex-id }
      (merge dex-info { active: active })
    )
    (ok true)
  )
)