! PR libgomp/59467
! { dg-do compile }
! { dg-options "-fopenmp" }
  FUNCTION t()
    INTEGER :: a, b, t
    a = 0
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE	! { dg-error "is not threadprivate or private in outer context" }
        !$OMP ATOMIC WRITE
        a = 6
      !$OMP END SINGLE COPYPRIVATE (a)
      b = a
    !$OMP END PARALLEL
    t = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE COPYPRIVATE (b)
    !$OMP END PARALLEL
    t = t + b
  END FUNCTION
