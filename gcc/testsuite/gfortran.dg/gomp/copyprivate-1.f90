! based on pr59467.f90 but COPYPRIVATE on the directive
! { dg-additional-options "-fdump-tree-original" }

  FUNCTION t()
    INTEGER :: a, b, t
    a = 0
    b = 0
    t = b
    b = 0
    !$OMP PARALLEL REDUCTION(+:b)
      !$OMP SINGLE COPYPRIVATE (b)
        !$OMP ATOMIC WRITE
        b = 6
      !$OMP END SINGLE
    !$OMP END PARALLEL
    t = t + b
  END FUNCTION

! { dg-final { scan-tree-dump-times "#pragma omp parallel reduction\\(\\+:b\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp single copyprivate\\(b\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed" 1 "original" } }
