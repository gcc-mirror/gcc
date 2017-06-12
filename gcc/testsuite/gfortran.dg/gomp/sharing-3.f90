! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo (vara, varb, varc, vard, n)
  integer :: n, vara(n), varb(*), varc(:), vard(6), vare(6)
  vare(:) = 0
  !$omp parallel default(none) shared(vara, varb, varc, vard, vare)
    !$omp master
      vara(1) = 1
      varb(1) = 1
      varc(1) = 1
      vard(1) = 1
      vare(1) = 1
    !$omp end master
  !$omp end parallel
  !$omp parallel default(none) private(vara, varc, vard, vare)
    vara(1) = 1
    varc(1) = 1
    vard(1) = 1
    vare(1) = 1
  !$omp end parallel
  !$omp parallel default(none) firstprivate(vara, varc, vard, vare)
    vara(1) = 1
    varc(1) = 1
    vard(1) = 1
    vare(1) = 1
  !$omp end parallel
  !$omp parallel default(none)	! { dg-error "enclosing 'parallel'" }
    !$omp master
      vara(1) = 1		! { dg-error "not specified" }
      varb(1) = 1		! Assumed-size is predetermined
      varc(1) = 1		! { dg-error "not specified" }
      vard(1) = 1		! { dg-error "not specified" }
      vare(1) = 1		! { dg-error "not specified" }
    !$omp end master
  !$omp end parallel
end subroutine foo
