! { dg-do compile }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:)
  integer i

  allocate (a(N))
  allocate (b(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc wait (1 2) ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (1,) ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (,1) ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (1, 2, ) ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (1, 2, ,) ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (1 ! { dg-error "Syntax error in OpenACC expression list at" }

  !$acc wait (1, *) ! { dg-error "Invalid argument to \\\$\\\!ACC WAIT" }

  !$acc wait (1, a) ! { dg-error "WAIT clause at \\\(1\\\) requires a scalar INTEGER expression" }

  !$acc wait (a) ! { dg-error "WAIT clause at \\\(1\\\) requires a scalar INTEGER expression" }

  !$acc wait (N) 

  !$acc wait (1.0) ! { dg-error "WAIT clause at \\\(1\\\) requires a scalar INTEGER expression" }

  !$acc wait 1 ! { dg-error "Unclassifiable OpenACC directive" }

  !$acc wait N ! { dg-error "Unclassifiable OpenACC directive" }

  !$acc wait (1)
end program asyncwait
