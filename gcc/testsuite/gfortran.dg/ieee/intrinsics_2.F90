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
  if (exponent(x) /= 0) STOP 1
  if (spacing(x) /= spacing(tiny(x))) STOP 2
  call check_positive_zero(rrspacing(x))
  call check_positive_zero(set_exponent(x,42))

  x = -0.
  call check_negative_zero(fraction(x))
  if (exponent(x) /= 0) STOP 3
  if (spacing(x) /= spacing(tiny(x))) STOP 4
  call check_positive_zero(rrspacing(x))
  call check_negative_zero(set_exponent(x,42))

  x = inf
  if (.not. isnan(fraction(x))) STOP 5
  if (exponent(x) /= huge(0)) STOP 6
  if (.not. isnan(spacing(x))) STOP 7
  if (.not. isnan(rrspacing(x))) STOP 8
  if (.not. isnan(set_exponent(x, 42))) STOP 9

  x = -inf
  if (.not. isnan(fraction(x))) STOP 10
  if (exponent(x) /= huge(0)) STOP 11
  if (.not. isnan(spacing(x))) STOP 12
  if (.not. isnan(rrspacing(x))) STOP 13
  if (.not. isnan(set_exponent(x, 42))) STOP 14

  x = nan
  if (.not. isnan(fraction(x))) STOP 15
  if (exponent(x) /= huge(0)) STOP 16
  if (.not. isnan(spacing(x))) STOP 17
  if (.not. isnan(rrspacing(x))) STOP 18
  if (.not. isnan(set_exponent(x, 42))) STOP 19

contains

  subroutine check_positive_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_positive_zero) STOP 20
  end

  subroutine check_negative_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_negative_zero) STOP 21
  end

end
