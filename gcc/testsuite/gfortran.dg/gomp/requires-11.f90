function foo (x, y) result (z)
  !$omp requires atomic_default_mem_order(release)
  implicit none
  real :: x, y, z

  !$omp atomic write
    x = y

  !$omp atomic update
    x = x + 1

  !$omp atomic read  ! { dg-error "!.OMP ATOMIC READ at .1. incompatible with RELEASE clause implicitly provided by a REQUIRES directive" }
    z = x
end

function bar (a, b) result (c)
  !$omp requires atomic_default_mem_order(acquire)
  implicit none
  real :: a, b, c

  !$omp atomic write  ! { dg-error "!.OMP ATOMIC WRITE at .1. incompatible with ACQUIRES clause implicitly provided by a REQUIRES directive" }
    a = b

  !$omp atomic update
    a = a + 1

  !$omp atomic read
    c = a
end


