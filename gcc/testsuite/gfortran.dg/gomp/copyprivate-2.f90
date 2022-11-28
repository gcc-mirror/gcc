  FUNCTION t()
    INTEGER :: a, b, t
    a = 0
    t = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE COPYPRIVATE (b) NOWAIT  ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE
    !$OMP END PARALLEL
    t = t + b
  END FUNCTION

  FUNCTION t2()
    INTEGER :: a, b, t2
    a = 0
    t2 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE NOWAIT COPYPRIVATE (b)  ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE
    !$OMP END PARALLEL
    t2 = t2 + b
  END FUNCTION

  FUNCTION t3()
    INTEGER :: a, b, t3
    a = 0
    t3 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE COPYPRIVATE (b)  ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE NOWAIT
    !$OMP END PARALLEL
    t3 = t3 + b
  END FUNCTION

  FUNCTION t4()
    INTEGER :: a, b, t4
    a = 0
    t4 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE NOWAIT COPYPRIVATE (b)  ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
    !$OMP END PARALLEL
    t4 = t4 + b
  END FUNCTION

  FUNCTION t5()
    INTEGER :: a, b, t5
    a = 0
    t5 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE COPYPRIVATE (b) NOWAIT  ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
    !$OMP END PARALLEL
    t5 = t5 + b
  END FUNCTION

  FUNCTION t6()
    INTEGER :: a, b, t6
    a = 0
    t6 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE NOWAIT
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE COPYPRIVATE (b) ! { dg-error "NOWAIT clause must not be used with COPYPRIVATE clause" }
    !$OMP END PARALLEL
    t6 = t6 + b
  END FUNCTION

  FUNCTION t7()
    INTEGER :: a, b, t7
    a = 0
    t7 = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE COPYPRIVATE (b)
        !$OMP ATOMIC WRITE
        b = 7
      !$OMP END SINGLE COPYPRIVATE (b) ! { dg-error "Symbol 'b' present on multiple clauses" }
    !$OMP END PARALLEL
    t7 = t7 + b
  END FUNCTION
