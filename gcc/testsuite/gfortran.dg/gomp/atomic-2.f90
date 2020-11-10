! { dg-do compile }

subroutine bar
  integer :: i, v
  real :: f
  !$omp atomic update acq_rel hint("abc") ! { dg-error "OMP ATOMIC UPDATE at .1. incompatible with ACQ_REL or ACQUIRE clauses" }
    ! { dg-error "HINT clause at .1. requires a scalar INTEGER expression" "" { target *-*-* } .-1 }
    ! { dg-error "Value of HINT clause at .1. shall be a valid constant hint expression" "" { target *-*-* } .-2 }
    i = i + 1
  !$omp end atomic

  !$omp atomic acq_rel ! { dg-error "OMP ATOMIC UPDATE at .1. incompatible with ACQ_REL or ACQUIRE clauses" }
  i = i + 1
  !$omp end atomic

  !$omp atomic capture,acq_rel , hint (1)
  i = i + 1
  v = i
  !$omp end atomic

  !$omp atomic acq_rel , hint (1), update ! { dg-error "OMP ATOMIC UPDATE at .1. incompatible with ACQ_REL or ACQUIRE clauses" }
  i = i + 1
  !$omp end atomic

  !$omp atomic hint(0),acquire capture
  i = i + 1
  v = i
  !$omp end atomic

  !$omp atomic write capture ! { dg-error "multiple atomic clauses" }
  i = 2
  v = i
  !$omp end atomic

  !$omp atomic foobar ! { dg-error "Failed to match clause" }
end

! moved here from atomic.f90
subroutine openmp51_foo
  integer :: x, v
  !$omp atomic update seq_cst capture  ! { dg-error "multiple atomic clauses" }
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic seq_cst, capture, update  ! { dg-error "multiple atomic clauses" }
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic capture, seq_cst ,update  ! { dg-error "multiple atomic clauses" }
  x = x + 2
  v = x
  !$omp end atomic
end

subroutine openmp51_bar
  integer :: i, v
  real :: f
  !$omp atomic relaxed capture update  ! { dg-error "multiple atomic clauses" }
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic update capture,release , hint (1)  ! { dg-error "multiple atomic clauses" }
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic hint(0),update relaxed capture  ! { dg-error "multiple atomic clauses" }
  i = i + 1
  v = i
  !$omp end atomic
end
