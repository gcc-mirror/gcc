! { dg-do run }
! { dg-options "-ff2c" }
! Verifies that internal functions are not broken by f2c calling conventions
program test
  real, target :: f
  real, pointer :: q
  real :: g
  f = 1.0
  q=>f
  g = foo(q)
  if (g .ne. 1.0) call abort
contains
function foo (p)
  real, pointer :: foo
  real, pointer :: p
  foo => p
end function
end program
