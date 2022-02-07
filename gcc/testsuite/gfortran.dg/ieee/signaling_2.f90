! { dg-do run { target { ! ia32 } } }
! x87 / x86-32 ABI is unsuitable for signaling NaNs
!
! { dg-require-effective-target issignaling } */
! The companion C source needs access to the issignaling macro.
!
! { dg-additional-sources signaling_2_c.c }
! { dg-additional-options "-w" }
! The -w option is needed to make cc1 not report a warning for
! the -fintrinsic-modules-path option passed by ieee.exp
!
program test
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface
    integer(kind=c_int) function isnansf (x) bind(c)
      import :: c_float, c_int
      real(kind=c_float), value :: x
    end function

    integer(kind=c_int) function isnans (x) bind(c)
      import :: c_double, c_int
      real(kind=c_double), value :: x
    end function

    integer(kind=c_int) function isnansl (x) bind(c)
      import :: c_long_double, c_int
      real(kind=c_long_double), value :: x
    end function
  end interface

  real(kind=c_float) :: x
  real(kind=c_double) :: y
  real(kind=c_long_double) :: z

  if (ieee_support_nan(x)) then
    x = ieee_value(x, ieee_signaling_nan)
    if (ieee_class(x) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(x)) stop 101
    if (isnansf(x) /= 1) stop 102

    x = ieee_value(x, ieee_quiet_nan)
    if (ieee_class(x) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(x)) stop 104
    if (isnansf(x) /= 0) stop 105
  end if

  if (ieee_support_nan(y)) then
    y = ieee_value(y, ieee_signaling_nan)
    if (ieee_class(y) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(y)) stop 101
    if (isnans(y) /= 1) stop 102

    y = ieee_value(y, ieee_quiet_nan)
    if (ieee_class(y) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(y)) stop 104
    if (isnans(y) /= 0) stop 105
  end if

  if (ieee_support_nan(z)) then
    z = ieee_value(z, ieee_signaling_nan)
    if (ieee_class(z) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(z)) stop 101
    if (isnansl(z) /= 1) stop 102

    z = ieee_value(z, ieee_quiet_nan)
    if (ieee_class(z) /= ieee_quiet_nan) stop 103
    if (.not. ieee_is_nan(z)) stop 104
    if (isnansl(z) /= 0) stop 105
  end if

end program test
