;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Functions:

;; Tank Invader Missile -> Tank Invader Missile
;; called to to update the game

(define (main g)
  (big-bang g               ;; Game 
    (on-tick game-tick)     ;; Game -> Game
    (to-draw game-render)   ;; Game -> Image
    (on-key key-handler)    ;; Game KeyEvent -> Game
    )
  )

;; Game -> Game
;; updates the position of each object
;; !!!
#;
(check-expect (game-tick (make-game (list (make-invader 100 100 0))
                                    (list (make-missile 150 300))
                                    (make-tank 100 0)))
                         (make-game (list (make-invader 100 (+ 100 INVADER-Y-SPEED) 0))
                                    (collide-missiles (update-missiles (list (make-missile 150 (+ 300 MISSILE-SPEED)))))
                                    (make-tank 100 0)))
#;
(check-expect (game-tick (make-game (list (make-invader 100 100 -3) (make-invader WIDTH 90 3))
                                    (list (make-missile 150 300) (make-missile 100 150))
                                    (make-tank 100 1)))
                         (make-game (list (make-invader 97 (+ 100 INVADER-Y-SPEED) -3) (make-invader WIDTH (+ 90 INVADER-Y-SPEED) -3))
                                    (collide-missiles (update-missiles (list (make-missile 150 (+ 300 MISSILE-SPEED)) (make-missile 100 (+ 150 MISSILE-SPEED)))))
                                    (make-tank 101 1)))
#;
(check-expect (game-tick (make-game empty
                                    empty
                                    (make-tank 0 -1)))
                         (make-game empty
                                    empty
                                    (make-tank 0 0)))

;; (define (game-tick g) ...)   ;stub

(define (game-tick g)
  (make-game (tick-invaders (game-invaders g))
             (collide-missiles (update-missiles (game-missiles g)) (game-invaders g))
             (tick-tank (game-tank g)))
  )


;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Invader -> Invader
;; updates the invader-x
(check-expect (tick-invaders empty) empty)
(check-expect (tick-invaders (list (make-invader 10 10 0)))
              (list (make-invader 10 (tick-invader-y 10) 0)))
(check-expect (tick-invaders (list (make-invader 10 10 1)))
              (list (make-invader 11 (tick-invader-y 10) 1)))
(check-expect (tick-invaders (list (make-invader 0 10 -1)))
              (list (make-invader 0 (tick-invader-y 10) 1)))
(check-expect (tick-invaders (list (make-invader WIDTH 10 1)))
              (list (make-invader WIDTH (tick-invader-y 10) -1)))
(check-expect (tick-invaders (list (make-invader 10 HEIGHT 1)))
              (list (make-invader 11 (tick-invader-y HEIGHT) 1)))

(define (tick-invaders i)
  (cond [(empty? i) empty]
        [else
         (cons (make-invader (tick-invader-x (invader-x (first i)) (invader-dx (first i)))
                             (tick-invader-y (invader-y (first i)))
                             (tick-invader-dx (invader-x (first i)) (invader-dx (first i))))
               (tick-invaders (rest i)))]
        )
  )


;; Number -> Number
;; updates invader-x
(check-expect (tick-invader-x 10 10) 20)
(check-expect (tick-invader-x 0 -10) 0)
(check-expect (tick-invader-x WIDTH 10) WIDTH)

(define (tick-invader-x x dx)
  (cond [(and (> x 0) (< x WIDTH)) (+ x dx)]
        [(<= x 0) 0]
        [(>= x WIDTH) WIDTH]
        )
)

;; Number -> Number
;; updates invader-dx
(check-expect (tick-invader-dx 10 10) 10)
(check-expect (tick-invader-dx 0 -10) 10)
(check-expect (tick-invader-dx WIDTH 10) -10)

(define (tick-invader-dx x dx)
  (if (and (> x 0) (< x WIDTH))
      dx
      (- dx))
)

;; Number -> Number
;; updates the invader-y
(check-expect (tick-invader-y 0) (+ 0 INVADER-Y-SPEED))
(check-expect (tick-invader-y HEIGHT) HEIGHT)

(define (tick-invader-y y)
  (if (< y HEIGHT)
      (+ y INVADER-Y-SPEED)
      HEIGHT)
  )

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Missiles -> Missiles
;; updates all missiles
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (list (make-missile 10 0))) empty)
(check-expect (update-missiles (list (make-missile 20 20)))
              (list (make-missile 20 10)))
(check-expect (update-missiles (list (make-missile 20 20) (make-missile 30 30)))
              (list (make-missile 20 10) (make-missile 30 20)))
(check-expect (update-missiles (list (make-missile 20 0) (make-missile 30 30)))
              (list (make-missile 30 20)))
(check-expect (update-missiles (list (make-missile 20 20) (make-missile 20 0)))
              (list (make-missile 20 10)))

(define (update-missiles m)
  (onscreen-missiles (tick-missiles m))
  )

;; Missiles -> Missiles
;; updates the postion of all missiles
(check-expect (tick-missiles empty) empty)
(check-expect (tick-missiles (list (make-missile 10 10)))
              (list (make-missile 10 (- 10 MISSILE-SPEED))))
(check-expect (tick-missiles (list (make-missile 10 10) (make-missile 20 20)))
              (list (make-missile 10 (- 10 MISSILE-SPEED)) (make-missile 20 (- 20 MISSILE-SPEED))))

(define (tick-missiles m)
  (cond [(empty? m) empty]
        [else
         (cons (tick-missile (first m))
               (tick-missiles (rest m)))]
        )
  )

;; Missile -> Missile
;; updates the position of missile
(check-expect (tick-missile (make-missile 10 10)) (make-missile 10 (- 10 MISSILE-SPEED)))

(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED))
  )

;; Missiles -> Missiles
;; removes all missile outside the screen
(check-expect (onscreen-missiles empty) empty)
(check-expect (onscreen-missiles (list (make-missile 10 10)))
              (list (make-missile 10 10)))
(check-expect (onscreen-missiles (list (make-missile 10 -10)
                                 (make-missile 10 10)))
              (list (make-missile 10 10)))
(check-expect (onscreen-missiles (list (make-missile 10 10)
                                 (make-missile 10 -10)))
              (list (make-missile 10 10)))

(define (onscreen-missiles m)
  (cond [(empty? m) empty]
        [else
         (if (>= (missile-y (first m)) 0)
             (cons (make-missile (missile-x (first m)) (missile-y (first m)))
                   (onscreen-missiles (rest m)))
             (onscreen-missiles (rest m))
             )]
        )
  )

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Missiles Invaders -> Missiles]
;; updates the missile list based on collision
(check-expect (collide-missiles (list (make-missile 10 100))
                             (list (make-invader 100 100 0)))
              (list (make-missile 10 100)))
(check-expect (collide-missiles (list (make-missile 100 100))
                             (list (make-invader 100 100 0)))
              empty)

(define (collide-missiles m i)
  (cond [(empty? m) empty]
        [else
         (if (check-col-mis (first m) i)
             (collide-missiles (rest m) i)
             (cons (first m) (collide-missiles (rest m) i)))]
        )
  )

;; Missile Invaders -> Boolean
;; returns true if missile collides with invader
(check-expect (check-col-mis (make-missile 10 100)
                             (list (make-invader 100 100 0)))
              false)
(check-expect (check-col-mis (make-missile 100 100)
                             (list (make-invader 100 100 0)))
              true)

(define (check-col-mis m i)
  (cond [(empty? i) false]
        [else
         (if (check-collision m (first i))
             true
             (check-col-mis m (rest i)))
             ]
        )
  )

;; Invader Missiles -> Boolean
;; returns true if invader collides with missile
(check-expect (check-col-mis (make-invader 100 100 0)
                             (list(make-missile 10 100) ))
              false)
(check-expect (check-col-mis (make-invader 100 100 0)
                             (list (make-missile 100 100)))
              true)

(define (check-col-inv i m)
  (cond [(empty? m) false]
        [else
         (if (check-collision (first m) i)
             true
             (check-col-inv i (rest m)))]
        )
  )

;; Missile Invader -> Boolean
;; returns true if missile is colliding with invader
(check-expect (check-collision (make-missile 10 100)
                               (make-invader 100 100 0))
              false)
(check-expect (check-collision (make-missile 100 100)
                               (make-invader 100 100 0))
              true)

(define (check-collision m i)
  (and (and (>= (missile-x m) (invader-x i))
            (<= (missile-x m) (+ (invader-x i) (image-width INVADER))))
       (and (>= (missile-y m) (invader-y i))
            (<= (missile-y m) (+ (invader-y i) (image-height INVADER))))
       )
  )

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Tank -> Tank
;; updates the postion of tank
;; !!!
(define (tick-tank t) ...)

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Game KeyEvent -> Game
;; produces a missile when " " is pressed
;; !!!
(define (key-handler g kevt) ...)

;-----------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------;

;; Game -> Image
;; renders the game
;; !!!
(define (game-render g) ...)