! { dg-do run }
! Test whether volatile statements and attributes are accepted
! PR fortran/29601
program volatile_test
  implicit none
  real :: l,m
  real, volatile :: r = 3.
  volatile :: l
  l = 4.0
  m = 3.0
end program volatile_test
