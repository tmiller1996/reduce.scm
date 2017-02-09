;;; reduce.scm
;;; Tim Miller
;;; reduce a hypergraph

;;; is 'a' a member of 'lat'
(define member?
  (lambda (a lat)
	(cond
	 ((null? lat) #f)
	 ((eq? (car lat) a) #t)
	 (else
	  (member? a (cdr lat))))))

;;; is set 'a' a subset of set 'b'
(define subset?
  (lambda (a b)
	(cond
	 ((null? a) #t)
	 ((member? (car a) b)
	  (subset? (cdr a) b))
	 (else #f))))

;;; is a a strict subset of b
(define strict-subset?
  (lambda (a b)
	(and
	 (subset? a b)
	 (not (subset? b a)))))

;;; classic filter function
(define filter
  (lambda (L fun?)
	(cond
	 ((null? L) L)
	 ((fun? (car L))
	  (cons (car L) (filter (cdr L) fun?)))
	 (else
	  (filter (cdr L) fun?)))))

;;; does L contain a superset of a
(define contains-superset?
  (lambda (a L)
	(cond
	 ((null? L) #f)
	 ((strict-subset? a (car L)) #t)
	 (else
	  (contains-superset? a (cdr L))))))

;;; reduce a set of sets
(define reduce
  (lambda (L)
	(filter L (lambda (z)
				(not (contains-superset? z L))))))

;;; examples
;;; (reduce '((1) (2) (3))) => ((1) (2) (3))
;;; (reduce '((1 2 3) (1) (2) (3))) => ((1 2 3))
;;; (reduce '((1 2 3) (1) (2) (3) (4))) => ((1 2 3) (4))
;;; (reduce '((1) (2) (3) (1 2 3) (2 3 4) (4))) => ((1 2 3) (2 3 4))
