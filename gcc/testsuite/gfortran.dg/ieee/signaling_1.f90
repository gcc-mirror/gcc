! { dg-do run { target { ! ia32 } } }
! x87 / x86-32 ABI is unsuitable for signaling NaNs
!
! { dg-additional-sources signaling_1_c.c }
! { dg-additional-options "-w" }
! The -w option is needed to make cc1 not report a warning for 
! the -fintrinsic-modules-path option passed by ieee.exp
!
program test
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface
    real(kind=c_float) function create_nansf () bind(c)
      import :: c_float
    end function

    real(kind=c_double) function create_nans () bind(c)
      import :: c_double
    end function

    real(kind=c_long_double) function create_nansl () bind(c)
      import :: c_long_double
    end function
  end interface

  real(kind=c_float) :: x
  real(kind=c_double) :: y
  real(kind=c_long_double) :: z

  if (ieee_support_nan(x)) then
    x = create_nansf()
    if (ieee_class(x) /= ieee_signaling_nan) stop 100
    if (.not. ieee_is_nan(x)) stop 101
    if (ieee_is_negative(x)) stop 102
    if (ieee_is_finite(x)) stop 103
    if (ieee_is_normal(x)) stop 104
    if (.not. ieee_unordered(x, x)) stop 105
    if (.not. ieee_unordered(x, 1._c_float)) stop 106

    x = ieee_value(x, ieee_quiet_nan)
    if (ieee_class(x) /= ieee_quiet_nan) stop 107
    if (.not. ieee_is_nan(x)) stop 108
    if (ieee_is_negative(x)) stop 109
    if (ieee_is_finite(x)) stop 110
    if (ieee_is_normal(x)) stop 111
    if (.not. ieee_unordered(x, x)) stop 112
    if (.not. ieee_unordered(x, 1._c_double)) stop 113
  end if

  if (ieee_support_nan(y)) then
    y = create_nans()
    if (ieee_class(y) /= ieee_signaling_nan) stop 200
    if (.not. ieee_is_nan(y)) stop 201
    if (ieee_is_negative(y)) stop 202
    if (ieee_is_finite(y)) stop 203
    if (ieee_is_normal(y)) stop 204
    if (.not. ieee_unordered(y, x)) stop 205
    if (.not. ieee_unordered(y, 1._c_double)) stop 206

    y = ieee_value(y, ieee_quiet_nan)
    if (ieee_class(y) /= ieee_quiet_nan) stop 207
    if (.not. ieee_is_nan(y)) stop 208
    if (ieee_is_negative(y)) stop 209
    if (ieee_is_finite(y)) stop 210
    if (ieee_is_normal(y)) stop 211
    if (.not. ieee_unordered(y, y)) stop 212
    if (.not. ieee_unordered(y, 1._c_double)) stop 213
  end if

  if (ieee_support_nan(z)) then
    z = create_nansl()
    if (ieee_class(z) /= ieee_signaling_nan) stop 300
    if (.not. ieee_is_nan(z)) stop 301
    if (ieee_is_negative(z)) stop 302
    if (ieee_is_finite(z)) stop 303
    if (ieee_is_normal(z)) stop 304
    if (.not. ieee_unordered(z, z)) stop 305
    if (.not. ieee_unordered(z, 1._c_long_double)) stop 306

    z = ieee_value(z, ieee_quiet_nan)
    if (ieee_class(z) /= ieee_quiet_nan) stop 307
    if (.not. ieee_is_nan(z)) stop 308
    if (ieee_is_negative(z)) stop 309
    if (ieee_is_finite(z)) stop 310
    if (ieee_is_normal(z)) stop 311
    if (.not. ieee_unordered(z, z)) stop 312
    if (.not. ieee_unordered(z, 1._c_double)) stop 313
  end if

end program test
