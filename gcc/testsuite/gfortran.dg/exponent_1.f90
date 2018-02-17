! { dg-do run }
! PR fortran/28276
! Original code submitted by Harald Anlauf
! Converted to Dejagnu for the testsuite by Steven G. Kargl
!
program gfcbug36
  implicit none
  real, parameter :: one = 1.0
  real :: a = one

  if (fraction(a) /= 0.5) STOP 1
  if (fraction(one) /= 0.5) STOP 2
  if (fraction(1.0) /= 0.5) STOP 3

  if (exponent(a) /= 1.0) STOP 4
  if (exponent(one) /= 1.0) STOP 5
  if (exponent (1.0) /= 1.0) STOP 6

  if (scale(fraction(a),   exponent(a))   / a   /= 1.) STOP 7
  if (scale(fraction(one), exponent(one)) / one /= 1.) STOP 8
  if (scale(fraction(1.0), exponent(1.0)) / 1.0 /= 1.) STOP 9

end program gfcbug36
