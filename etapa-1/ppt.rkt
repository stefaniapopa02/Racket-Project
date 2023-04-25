#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X) 0
      (+ (* (first X) (first Y)) (dot-product (rest X) (rest Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (multiply-helper M V '()))

(define (multiply-helper M V acc)
  (if (null? M) (reverse acc)
      (multiply-helper (rest M) V (cons (dot-product (first M) V) acc))))
  


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

(define (get-interval n power) ;se va apela cu 1
  (if (<= n (/ (- power 1) 2)) (cons (/ (+ (/ power 3) 1) 2) (list (/ (- power 1) 2)))
      (get-interval n (* power 3))))
;(get-interval 64 1)

(define (which-third n a b)
  (cond ((<= n (+ (- a 1) (/ (- b (- a 1)) 3))) 1)
        ((<= n (+ (- a 1) (* 2 (/ (- b (- a 1)) 3)))) 2)
        (else 3)))
  
;(which-third 64 41 67)


(define (which-T n a b)
  (cond ((eq? a b) '())
        ((eq? (which-third n a b) 1) (append (which-T n a (+ (- a 1) (/ (- b (- a 1)) 3))) (list 1)))
        ((eq? (which-third n a b) 2) (append (which-T n (+ a (/ (- b (- a 1)) 3)) (+ (- a 1) (* 2 (/ (- b (- a 1)) 3)))) (list 2)))
        ((eq? (which-third n a b) 3) (append (which-T n (+ a (* 2 (/ (- b (- a 1)) 3))) b) (list 3)))))

;(which-T 64 41 121)

(define (get-transformations n)
  (reverse (which-T n (car (get-interval n 1)) (car (cdr (get-interval n 1))))))


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (amt-helper Ts ppt ppt))

(define (amt-helper Ts ppt new_ppt)
  (cond ((null? Ts) new_ppt)
        ((eq? (first Ts) 1) (amt-helper (rest Ts) ppt (multiply T1 new_ppt)))
        ((eq? (first Ts) 2) (amt-helper (rest Ts) ppt (multiply T2 new_ppt)))
        ((eq? (first Ts) 3) (amt-helper (rest Ts) ppt (multiply T3 new_ppt)))))
    


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
