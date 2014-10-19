! { dg-do run }
! { dg-additional-options "-fno-range-check" }
!
! Check handling of special values by FRACTION, EXPONENT,
! SPACING, RRSPACING and SET_EXPONENT.

program test
  implicit none
  real, parameter :: inf = 2 * huge(0.)
  real, parameter :: nan = 0. / 0.

  real, volatile :: x

  x = 0.
  call check_positive_zero(fraction(x))
  if (exponent(x) /= 0) call abort
  if (spacing(x) /= spacing(tiny(x))) call abort
  call check_positive_zero(rrspacing(x))
  call check_positive_zero(set_exponent(x,42))

  x = -0.
  call check_negative_zero(fraction(x))
  if (exponent(x) /= 0) call abort
  if (spacing(x) /= spacing(tiny(x))) call abort
  call check_positive_zero(rrspacing(x))
  call check_negative_zero(set_exponent(x,42))

  x = inf
  if (.not. isnan(fraction(x))) call abort
  if (exponent(x) /= huge(0)) call abort
  if (.not. isnan(spacing(x))) call abort
  if (.not. isnan(rrspacing(x))) call abort
  if (.not. isnan(set_exponent(x, 42))) call abort

  x = -inf
  if (.not. isnan(fraction(x))) call abort
  if (exponent(x) /= huge(0)) call abort
  if (.not. isnan(spacing(x))) call abort
  if (.not. isnan(rrspacing(x))) call abort
  if (.not. isnan(set_exponent(x, 42))) call abort

  x = nan
  if (.not. isnan(fraction(x))) call abort
  if (exponent(x) /= huge(0)) call abort
  if (.not. isnan(spacing(x))) call abort
  if (.not. isnan(rrspacing(x))) call abort
  if (.not. isnan(set_exponent(x, 42))) call abort

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
