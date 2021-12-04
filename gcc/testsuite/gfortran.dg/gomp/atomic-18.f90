module m
implicit none
integer i, v
real f
contains
subroutine foo (j)
integer, value :: j
  !$omp atomic update,update        ! { dg-error "Duplicated atomic clause: unexpected update clause" }
  i = i + 1
  !$omp atomic seq_cst release      ! { dg-error "Duplicated memory-order clause: unexpected release clause" }
  i = i + 1
  !$omp atomic read,release         ! { dg-error "ATOMIC READ at .1. incompatible with RELEASE clause" }
  v = i
  !$omp atomic acquire , write      ! { dg-error "ATOMIC WRITE at .1. incompatible with ACQUIRE clause" }
  i = v
  !$omp atomic capture hint (0) capture  ! { dg-error "Duplicated 'capture' clause" }
  v = i = i + 1
  !$omp atomic hint(j + 2)      ! { dg-error "Value of HINT clause at .1. shall be a valid constant hint expression" }
  i = i + 1
  !$omp atomic hint(f)
    ! { dg-error "HINT clause at .1. requires a scalar INTEGER expression" "" { target *-*-* } .-1 }
    ! { dg-error "Value of HINT clause at .1. shall be a valid constant hint expression" "" { target *-*-* } .-2 }
  i = i + 1
  !$omp atomic foobar           ! { dg-error "Failed to match clause" }
  i = i + 1
end
end module
