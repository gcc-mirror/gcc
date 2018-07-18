! { dg-do run }
!
! Testing IEEE modules on large real kinds

program test

  use ieee_arithmetic
  implicit none

  ! k1 and k2 will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: k1 = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: k2 = &
    max(ieee_selected_real_kind(precision(0._k1) + 1), kind(0.d0))

  real(kind=k1) :: x1, y1
  real(kind=k2) :: x2, y2
  logical :: l

  ! Checking ieee_is_finite

  if (.not. ieee_is_finite(huge(0._k1))) STOP 1
  if (ieee_is_finite(ieee_value(0._k1, ieee_negative_inf))) STOP 2
  x1 = -42
  if (.not. ieee_is_finite(x1)) STOP 3
  if (ieee_is_finite(sqrt(x1))) STOP 4

  if (.not. ieee_is_finite(huge(0._k2))) STOP 5
  if (ieee_is_finite(ieee_value(0._k2, ieee_negative_inf))) STOP 6
  x2 = -42
  if (.not. ieee_is_finite(x2)) STOP 7
  if (ieee_is_finite(sqrt(x2))) STOP 8

  ! Other ieee_is intrinsics

  if (ieee_is_nan(huge(0._k1))) STOP 9
  if (.not. ieee_is_negative(-huge(0._k1))) STOP 10
  if (.not. ieee_is_normal(-huge(0._k1))) STOP 11

  if (ieee_is_nan(huge(0._k2))) STOP 12
  if (.not. ieee_is_negative(-huge(0._k2))) STOP 13
  if (.not. ieee_is_normal(-huge(0._k2))) STOP 14

  ! ieee_support intrinsics

  if (.not. ieee_support_datatype(x1)) STOP 15
  if (.not. ieee_support_denormal(x1)) STOP 16
  if (.not. ieee_support_divide(x1)) STOP 17
  if (.not. ieee_support_inf(x1)) STOP 18
  if (.not. ieee_support_io(x1)) STOP 19
  if (.not. ieee_support_nan(x1)) STOP 20
  if (.not. ieee_support_rounding(ieee_nearest, x1)) STOP 21
  if (.not. ieee_support_sqrt(x1)) STOP 22
  if (.not. ieee_support_standard(x1)) STOP 23

  l = ieee_support_underflow_control(x1)

  if (.not. ieee_support_datatype(x2)) STOP 24
  if (.not. ieee_support_denormal(x2)) STOP 25
  if (.not. ieee_support_divide(x2)) STOP 26
  if (.not. ieee_support_inf(x2)) STOP 27
  if (.not. ieee_support_io(x2)) STOP 28
  if (.not. ieee_support_nan(x2)) STOP 29
  if (.not. ieee_support_rounding(ieee_nearest, x2)) STOP 30
  if (.not. ieee_support_sqrt(x2)) STOP 31
  if (.not. ieee_support_standard(x2)) STOP 32

  l = ieee_support_underflow_control(x2)

  ! ieee_value and ieee_class

  if (.not. ieee_is_nan(ieee_value(x1, ieee_quiet_nan))) STOP 33
  if (ieee_class(ieee_value(x1, ieee_positive_denormal)) &
    /= ieee_positive_denormal) STOP 34

  if (.not. ieee_is_nan(ieee_value(x2, ieee_quiet_nan))) STOP 35
  if (ieee_class(ieee_value(x2, ieee_positive_denormal)) &
    /= ieee_positive_denormal) STOP 36

  ! ieee_unordered

  if (.not. ieee_unordered(ieee_value(x1, ieee_quiet_nan), 0._k1)) STOP 37
  if (ieee_unordered(ieee_value(x1, ieee_negative_inf), 0._k1)) STOP 38

  if (.not. ieee_unordered(ieee_value(x2, ieee_quiet_nan), 0._k2)) STOP 39
  if (ieee_unordered(ieee_value(x2, ieee_negative_inf), 0._k2)) STOP 40

  ! ieee_copy_sign

  if (.not. ieee_class(ieee_copy_sign(ieee_value(x1, ieee_positive_inf), -1.)) &
            == ieee_negative_inf) STOP 41
  if (.not. ieee_class(ieee_copy_sign(0._k1, -42._k2)) &
            == ieee_negative_zero) STOP 42

  if (.not. ieee_class(ieee_copy_sign(ieee_value(x2, ieee_positive_inf), -1.)) &
            == ieee_negative_inf) STOP 43
  if (.not. ieee_class(ieee_copy_sign(0._k2, -42._k1)) &
            == ieee_negative_zero) STOP 44

  ! ieee_logb

  if (ieee_logb (42._k1) /= exponent(42._k1) - 1) STOP 45

  if (ieee_logb (42._k2) /= exponent(42._k2) - 1) STOP 46

  ! ieee_next_after

  if (ieee_next_after(42._k1, ieee_value(x1, ieee_positive_inf)) &
      /= 42._k1 + spacing(42._k1)) STOP 47

  if (ieee_next_after(42._k2, ieee_value(x2, ieee_positive_inf)) &
      /= 42._k2 + spacing(42._k2)) STOP 48

  ! ieee_rem

  if (ieee_class(ieee_rem(-42._k1, 2._k1)) /= ieee_negative_zero) &
    STOP 49

  if (ieee_class(ieee_rem(-42._k2, 2._k2)) /= ieee_negative_zero) &
    STOP 50

  ! ieee_rint

  if (ieee_rint(-1.1_k1) /= -1._k1) STOP 51
  if (ieee_rint(huge(x1)) /= huge(x1)) STOP 52

  if (ieee_rint(-1.1_k2) /= -1._k2) STOP 53
  if (ieee_rint(huge(x2)) /= huge(x2)) STOP 54

  ! ieee_scalb

  x1 = sqrt(42._k1)
  if (ieee_scalb(x1, 2) /= 4._k1 * x1) STOP 55
  if (ieee_scalb(x1, -2) /= x1 / 4._k1) STOP 56

  x2 = sqrt(42._k2)
  if (ieee_scalb(x2, 2) /= 4._k2 * x2) STOP 57
  if (ieee_scalb(x2, -2) /= x2 / 4._k2) STOP 58

end program test
