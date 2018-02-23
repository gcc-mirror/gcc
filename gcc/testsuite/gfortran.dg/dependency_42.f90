! { dg-do run }
! { dg-options "-Warray-temporaries" }
! PR fortran/56937 - unnecessary temporaries with vector indices
program main
  real :: q(4), r(4), p(3)
  integer :: idx(3)
  p = [0.5, 1.0, 2.0]
  idx = [4,3,1]
  r = 1.0
  r(idx) = r(idx) + p
  q = 1.0
  q(4) = q(4) + p(1)
  q(3) = q(3) + p(2)
  q(1) = q(1) + p(3)
  if (any (q - r /= 0)) STOP 1
end
