! { dg-do run }
! { dg-additional-options "-fno-range-check" }
!
! Check compile-time simplification of functions FRACTION, EXPONENT,
! SPACING, RRSPACING and SET_EXPONENT for special values.

program test
  implicit none
  real, parameter :: inf = 2 * huge(0.)
  real, parameter :: nan = 0. / 0.

  call check_positive_zero(fraction(0.))
  call check_negative_zero(fraction(-0.))
  if (.not. isnan(fraction(inf))) call abort
  if (.not. isnan(fraction(-inf))) call abort
  if (.not. isnan(fraction(nan))) call abort

  if (exponent(0.) /= 0) call abort
  if (exponent(-0.) /= 0) call abort
  if (exponent(inf) /= huge(0)) call abort
  if (exponent(-inf) /= huge(0)) call abort
  if (exponent(nan) /= huge(0)) call abort

  if (spacing(0.) /= spacing(tiny(0.))) call abort
  if (spacing(-0.) /= spacing(tiny(0.))) call abort
  if (.not. isnan(spacing(inf))) call abort
  if (.not. isnan(spacing(-inf))) call abort
  if (.not. isnan(spacing(nan))) call abort

  call check_positive_zero(rrspacing(0.))
  call check_positive_zero(rrspacing(-0.))
  if (.not. isnan(rrspacing(inf))) call abort
  if (.not. isnan(rrspacing(-inf))) call abort
  if (.not. isnan(rrspacing(nan))) call abort

  call check_positive_zero(set_exponent(0.,42))
  call check_negative_zero(set_exponent(-0.,42))
  if (.not. isnan(set_exponent(inf, 42))) call abort
  if (.not. isnan(set_exponent(-inf, 42))) call abort
  if (.not. isnan(set_exponent(nan, 42))) call abort

contains

  subroutine check_positive_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_positive_zero) call abort
  end

  subroutine check_negative_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_negative_zero) call abort
  end

end
