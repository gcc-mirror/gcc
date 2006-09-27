! { dg-do run }
! PR fortran/28276
! Original code submitted by Harald Anlauf
! Converted to Dejagnu for the testsuite by Steven G. Kargl
!
program gfcbug36
  implicit none
  real, parameter :: one = 1.0
  real :: a = one

  if (fraction(a) /= 0.5) call abort
  if (fraction(one) /= 0.5) call abort
  if (fraction(1.0) /= 0.5) call abort

  if (exponent(a) /= 1.0) call abort
  if (exponent(one) /= 1.0) call abort
  if (exponent (1.0) /= 1.0) call abort

  if (scale(fraction(a),   exponent(a))   / a   /= 1.) call abort
  if (scale(fraction(one), exponent(one)) / one /= 1.) call abort
  if (scale(fraction(1.0), exponent(1.0)) / 1.0 /= 1.) call abort

end program gfcbug36
