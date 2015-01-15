! { dg-do compile }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:)
  integer i

  allocate (a(N))
  allocate (b(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc wait async (1 2) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1,) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (,1) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1, 2, ) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1, 2, ,) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1 ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1, *) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (1, a) ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }

  !$acc wait async (a) ! { dg-error "ASYNC clause at \\\(1\\\) requires a scalar INTEGER expression" }

  !$acc wait async (N)

  !$acc wait async (1.0) ! { dg-error "ASYNC clause at \\\(1\\\) requires a scalar INTEGER expression" }

  !$acc wait async 1 ! { dg-error "Unexpected junk in \\\!\\\$ACC WAIT at" }
end program asyncwait
