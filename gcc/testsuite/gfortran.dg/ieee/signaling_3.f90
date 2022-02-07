! { dg-do run { target { ! ia32 } } }
! x87 / x86-32 ABI is unsuitable for signaling NaNs
!
program test
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic
  implicit none

  real(kind=c_float) :: x
  real(kind=c_double) :: y
  real(kind=c_long_double) :: z

  if (ieee_support_nan(x)) then
    x = ieee_value(x, ieee_signaling_nan)
    if (ieee_class(x) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(x)) stop 101

    x = ieee_value(x, ieee_quiet_nan)
    if (ieee_class(x) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(x)) stop 104
  end if

  if (ieee_support_nan(y)) then
    y = ieee_value(y, ieee_signaling_nan)
    if (ieee_class(y) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(y)) stop 101

    y = ieee_value(y, ieee_quiet_nan)
    if (ieee_class(y) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(y)) stop 104
  end if

  if (ieee_support_nan(z)) then
    z = ieee_value(z, ieee_signaling_nan)
    if (ieee_class(z) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(z)) stop 101

    z = ieee_value(z, ieee_quiet_nan)
    if (ieee_class(z) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(z)) stop 104
  end if

end program test
