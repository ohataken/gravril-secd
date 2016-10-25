(define (comp expr env code)
  (cond
    ((self-evaluation? expr)
      (list* 'ldc expr code))
    ((symbol? expr)
      (let ((pos (location expr env)))
        (if pos
          (list* 'ld pos code)
          (list* 'ldg expr code))))
    ((eq? (car expr) 'quote)
      (list* 'ldc (cadr expr) code))
    ((eq? (car expr) 'if)
      (let (
        (t-clause (comp (caddr expr) env '(join)))
        (f-clause (
          if (null? (cdddr expr))
            (list 'ldc '*undef 'join)
          (comp (cadddr expr) env '(join)))))
        (comp (cadr expr) env (list* 'sel t-clause f-clause code))))
    ((eq? (car expr) 'lambda)
      (let ((body (comp-body (cddr expr) (cons (cadr expr) env) '(rtn))))
        (list* 'ldf body code)))
    ((eq? (car expr) 'define)
      (comp (caddr expr) env (list* 'def (cadr expr) code)))
    (else
      (complis
        (cdr expr)
        env
        (list* 'args (length (cdr expr)))
        (comp (car expr) env (cons'app code))))))

(define (compile expr)
  (comp expr '() '(stop)))

(define (complis expr env code)
  (if (null? expr)
    code
    (comp (car expr) env (complis (cdr expr) env code))))

(define (comp-body body env code)
  (if (null? (cdr body))
    (comp (car body) env code)
    (comp (car body)
      env
      (list* 'pop (comp-body (cdr body) env code)))))

(define (position-var sym ls)
  (let loop ((i 0) (ls ls))
    (cond ((null? ls) #f)
    ((symbol? ls)
      (if (eq? sym ls) (- (+ i 1)) #f))
      ((eq? sym(car ls)) i)
      (else (loop (+i 1) (cdr ls))))))

(define (location sym ls)
  (let loop ((i 0) (ls ls))
    (if (null? ls) #f
      (let ((j (position-var sym (car ls))))
        (if j (cons i j)
          (loop (+ i 1) (cdr ls)))))))

(define (self-evaluation? expr)
  (and (not (pair? expr)) (not (symbol? expr))))
