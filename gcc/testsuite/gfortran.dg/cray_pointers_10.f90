! { dg-do run }
! { dg-options "-fcray-pointer" }
!
! PR fortran/45187
!
module foo
  implicit none
  real :: a
  pointer(c_a, a)
end module foo

program test
  use foo
  real :: z
  c_a = loc(z)
  a = 42
  if (z /= 42) STOP 1
end program test
