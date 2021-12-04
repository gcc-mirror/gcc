! { dg-do compile }

subroutine bar
  integer :: i, v
  real :: f
  !$omp atomic update acq_rel hint("abc")
    ! { dg-error "HINT clause at .1. requires a scalar INTEGER expression" "" { target *-*-* } .-1 }
    ! { dg-error "Value of HINT clause at .1. shall be a valid constant hint expression" "" { target *-*-* } .-2 }
    i = i + 1
  !$omp end atomic

  !$omp atomic acq_rel
  i = i + 1
  !$omp end atomic

  !$omp atomic capture,acq_rel , hint (1)
  i = i + 1
  v = i
  !$omp end atomic

  !$omp atomic acq_rel , hint (1), update
  i = i + 1
  !$omp end atomic

  !$omp atomic hint(0),acquire capture
  i = i + 1
  v = i
  !$omp end atomic

  !$omp atomic write capture ! { dg-error "with CAPTURE clause is incompatible with READ or WRITE" }
  i = 2
  v = i
  !$omp end atomic

  !$omp atomic foobar ! { dg-error "Failed to match clause" }
end
