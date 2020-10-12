! { dg-do run }
! { dg-options "-O2 -std=gnu" }
! PR fortran/96613 - Fix type/kind of temporaries evaluating MIN/MAX

program test
  implicit none
  real :: x = 7.7643945e+09
  real :: y = 6000.
  integer :: ix

  ix = min1 (5000.0, x)
  if (ix /= 5000) stop 1
  ix = min1 (y, x, 5555.d0)
  if (ix /= 5555) stop 2
end program
