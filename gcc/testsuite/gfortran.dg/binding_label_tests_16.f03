! { dg-do run }
! Verify that the variables 'a' in both modules don't collide.
module m
  use iso_c_binding
  implicit none
  integer(c_int), save, bind(C, name="") :: a = 5
end module m

module n
  use iso_c_binding
  implicit none
  integer(c_int), save, bind(C,name="") :: a = -5
end module n

program prog
use m
use n, b=>a
implicit none
  print *, a, b
  if (a /= 5 .or. b /= -5) STOP 1
end program prog
