! { dg-do run }

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_exceptions
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface check_equal
    procedure check_equal_float, check_equal_double
  end interface

  interface check_not_equal
    procedure check_not_equal_float, check_not_equal_double
  end interface

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  type(ieee_round_type) :: mode

  ! Test IEEE_COPY_SIGN
  sx1 = 1.3
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= sx1) call abort
  if (ieee_copy_sign(sx1, -1.) /= -sx1) call abort
  sx1 = huge(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= sx1) call abort
  if (ieee_copy_sign(sx1, -1.) /= -sx1) call abort
  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= sx1) call abort
  if (ieee_copy_sign(sx1, -1.) /= -sx1) call abort
  sx1 = tiny(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= sx1) call abort
  if (ieee_copy_sign(sx1, -1.) /= -sx1) call abort
  sx1 = tiny(sx1)
  sx1 = sx1 / 101
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= sx1) call abort
  if (ieee_copy_sign(sx1, -1.) /= -sx1) call abort

  sx1 = -1.3
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) call abort
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) call abort
  sx1 = -huge(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) call abort
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) call abort
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) call abort
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) call abort
  sx1 = -tiny(sx1)
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) call abort
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) call abort
  sx1 = -tiny(sx1)
  sx1 = sx1 / 101
  if (ieee_copy_sign(sx1, sx1) /= sx1) call abort
  if (ieee_copy_sign(sx1, -sx1) /= -sx1) call abort
  if (ieee_copy_sign(sx1, 1.) /= abs(sx1)) call abort
  if (ieee_copy_sign(sx1, -1.) /= -abs(sx1)) call abort

  if (ieee_class(ieee_copy_sign(0., -1.)) /= ieee_negative_zero) call abort
  if (ieee_class(ieee_copy_sign(-0., -1.)) /= ieee_negative_zero) call abort
  if (ieee_class(ieee_copy_sign(0., 1.)) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_copy_sign(-0., 1.)) /= ieee_positive_zero) call abort

  sx1 = ieee_value(0., ieee_quiet_nan)
  if (ieee_class(ieee_copy_sign(sx1, 1.)) /= ieee_quiet_nan) call abort
  if (ieee_class(ieee_copy_sign(sx1, -1.)) /= ieee_quiet_nan) call abort

  dx1 = 1.3
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.) /= dx1) call abort
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) call abort
  dx1 = huge(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.d0) /= dx1) call abort
  if (ieee_copy_sign(dx1, -1.) /= -dx1) call abort
  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.) /= dx1) call abort
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) call abort
  dx1 = tiny(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.d0) /= dx1) call abort
  if (ieee_copy_sign(dx1, -1.) /= -dx1) call abort
  dx1 = tiny(dx1)
  dx1 = dx1 / 101
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.) /= dx1) call abort
  if (ieee_copy_sign(dx1, -1.d0) /= -dx1) call abort

  dx1 = -1.3d0
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) call abort
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) call abort
  dx1 = -huge(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.) /= abs(dx1)) call abort
  if (ieee_copy_sign(dx1, -1.d0) /= -abs(dx1)) call abort
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) call abort
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) call abort
  dx1 = -tiny(dx1)
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.) /= abs(dx1)) call abort
  if (ieee_copy_sign(dx1, -1.d0) /= -abs(dx1)) call abort
  dx1 = -tiny(dx1)
  dx1 = dx1 / 101
  if (ieee_copy_sign(dx1, dx1) /= dx1) call abort
  if (ieee_copy_sign(dx1, -dx1) /= -dx1) call abort
  if (ieee_copy_sign(dx1, 1.d0) /= abs(dx1)) call abort
  if (ieee_copy_sign(dx1, -1.) /= -abs(dx1)) call abort

  if (ieee_class(ieee_copy_sign(0.d0, -1.)) /= ieee_negative_zero) call abort
  if (ieee_class(ieee_copy_sign(-0.d0, -1.)) /= ieee_negative_zero) call abort
  if (ieee_class(ieee_copy_sign(0.d0, 1.)) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_copy_sign(-0.d0, 1.)) /= ieee_positive_zero) call abort

  dx1 = ieee_value(0.d0, ieee_quiet_nan)
  if (ieee_class(ieee_copy_sign(dx1, 1.d0)) /= ieee_quiet_nan) call abort
  if (ieee_class(ieee_copy_sign(dx1, -1.)) /= ieee_quiet_nan) call abort

  ! Test IEEE_LOGB

  if (ieee_logb(1.17) /= exponent(1.17) - 1) call abort
  if (ieee_logb(-1.17) /= exponent(-1.17) - 1) call abort
  if (ieee_logb(huge(sx1)) /= exponent(huge(sx1)) - 1) call abort
  if (ieee_logb(-huge(sx1)) /= exponent(-huge(sx1)) - 1) call abort
  if (ieee_logb(tiny(sx1)) /= exponent(tiny(sx1)) - 1) call abort
  if (ieee_logb(-tiny(sx1)) /= exponent(-tiny(sx1)) - 1) call abort

  if (ieee_class(ieee_logb(0.)) /= ieee_negative_inf) call abort
  if (ieee_class(ieee_logb(-0.)) /= ieee_negative_inf) call abort

  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_class(ieee_logb(sx1)) /= ieee_positive_inf) call abort
  if (ieee_class(ieee_logb(-sx1)) /= ieee_positive_inf) call abort

  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_logb(sx1)) /= ieee_quiet_nan) call abort

  if (ieee_logb(1.17d0) /= exponent(1.17d0) - 1) call abort
  if (ieee_logb(-1.17d0) /= exponent(-1.17d0) - 1) call abort
  if (ieee_logb(huge(dx1)) /= exponent(huge(dx1)) - 1) call abort
  if (ieee_logb(-huge(dx1)) /= exponent(-huge(dx1)) - 1) call abort
  if (ieee_logb(tiny(dx1)) /= exponent(tiny(dx1)) - 1) call abort
  if (ieee_logb(-tiny(dx1)) /= exponent(-tiny(dx1)) - 1) call abort

  if (ieee_class(ieee_logb(0.d0)) /= ieee_negative_inf) call abort
  if (ieee_class(ieee_logb(-0.d0)) /= ieee_negative_inf) call abort

  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_class(ieee_logb(dx1)) /= ieee_positive_inf) call abort
  if (ieee_class(ieee_logb(-dx1)) /= ieee_positive_inf) call abort

  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_logb(dx1)) /= ieee_quiet_nan) call abort

  ! Test IEEE_NEXT_AFTER

  if (ieee_next_after(0.12, 1.0) /= nearest(0.12, 1.0)) call abort
  if (ieee_next_after(0.12, -1.0) /= nearest(0.12, -1.0)) call abort

  sx1 = 0.12
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = -0.12
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = huge(sx1)
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = tiny(sx1)
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = 0
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_next_after(sx1, sx1) /= sx1) call abort
  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_next_after(sx1, sx1)) /= ieee_quiet_nan) call abort

  if (ieee_next_after(0., 1.0) <= 0) call abort
  if (ieee_next_after(0., -1.0) >= 0) call abort
  sx1 = ieee_next_after(huge(sx1), ieee_value(sx1, ieee_negative_inf))
  if (.not. sx1 < huge(sx1)) call abort
  sx1 = ieee_next_after(huge(sx1), ieee_value(sx1, ieee_positive_inf))
  if (ieee_class(sx1) /= ieee_positive_inf) call abort
  sx1 = ieee_next_after(-tiny(sx1), 1.0)
  if (ieee_class(sx1) /= ieee_negative_denormal) call abort

  if (ieee_next_after(0.12d0, 1.0d0) /= nearest(0.12d0, 1.0)) call abort
  if (ieee_next_after(0.12d0, -1.0) /= nearest(0.12d0, -1.0)) call abort

  dx1 = 0.12
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = -0.12
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = huge(dx1)
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = tiny(dx1)
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = 0
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_next_after(dx1, dx1) /= dx1) call abort
  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_next_after(dx1, dx1)) /= ieee_quiet_nan) call abort

  if (ieee_next_after(0.d0, 1.0) <= 0) call abort
  if (ieee_next_after(0.d0, -1.0d0) >= 0) call abort
  dx1 = ieee_next_after(huge(dx1), ieee_value(dx1, ieee_negative_inf))
  if (.not. dx1 < huge(dx1)) call abort
  dx1 = ieee_next_after(huge(dx1), ieee_value(dx1, ieee_positive_inf))
  if (ieee_class(dx1) /= ieee_positive_inf) call abort
  dx1 = ieee_next_after(-tiny(dx1), 1.0d0)
  if (ieee_class(dx1) /= ieee_negative_denormal) call abort

  ! Test IEEE_REM

  if (ieee_rem(4.0, 3.0) /= 1.0) call abort
  if (ieee_rem(-4.0, 3.0) /= -1.0) call abort
  if (ieee_rem(2.0, 3.0d0) /= -1.0d0) call abort
  if (ieee_rem(-2.0, 3.0d0) /= 1.0d0) call abort
  if (ieee_rem(2.0d0, 3.0d0) /= -1.0d0) call abort
  if (ieee_rem(-2.0d0, 3.0d0) /= 1.0d0) call abort

  if (ieee_class(ieee_rem(ieee_value(0., ieee_quiet_nan), 1.0)) &
      /= ieee_quiet_nan) call abort
  if (ieee_class(ieee_rem(1.0, ieee_value(0.d0, ieee_quiet_nan))) &
      /= ieee_quiet_nan) call abort

  if (ieee_class(ieee_rem(ieee_value(0., ieee_positive_inf), 1.0)) &
      /= ieee_quiet_nan) call abort
  if (ieee_class(ieee_rem(ieee_value(0.d0, ieee_negative_inf), 1.0)) &
      /= ieee_quiet_nan) call abort
  if (ieee_rem(-1.0, ieee_value(0., ieee_positive_inf)) &
      /= -1.0) call abort
  if (ieee_rem(1.0, ieee_value(0.d0, ieee_negative_inf)) &
      /= 1.0) call abort


  ! Test IEEE_RINT

  if (ieee_support_rounding (ieee_nearest, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_nearest)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) call abort
  end if

  if (ieee_support_rounding (ieee_up, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_up)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 3) call abort
  end if

  if (ieee_support_rounding (ieee_down, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_down)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) call abort
  end if

  if (ieee_support_rounding (ieee_to_zero, sx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_to_zero)
    sx1 = 7 / 3.
    sx1 = ieee_rint (sx1)
    call ieee_set_rounding_mode (mode)
    if (sx1 /= 2) call abort
  end if

  if (ieee_class(ieee_rint(0.)) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_rint(-0.)) /= ieee_negative_zero) call abort

  if (ieee_support_rounding (ieee_nearest, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_nearest)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) call abort
  end if

  if (ieee_support_rounding (ieee_up, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_up)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 3) call abort
  end if

  if (ieee_support_rounding (ieee_down, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_down)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) call abort
  end if

  if (ieee_support_rounding (ieee_to_zero, dx1)) then
    call ieee_get_rounding_mode (mode)
    call ieee_set_rounding_mode (ieee_to_zero)
    dx1 = 7 / 3.d0
    dx1 = ieee_rint (dx1)
    call ieee_set_rounding_mode (mode)
    if (dx1 /= 2) call abort
  end if

  if (ieee_class(ieee_rint(0.d0)) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_rint(-0.d0)) /= ieee_negative_zero) call abort

  ! Test IEEE_SCALB

  sx1 = 1
  if (ieee_scalb(sx1, 2) /= 4.) call abort
  if (ieee_scalb(-sx1, 2) /= -4.) call abort
  if (ieee_scalb(sx1, -2) /= 1/4.) call abort
  if (ieee_scalb(-sx1, -2) /= -1/4.) call abort
  if (ieee_class(ieee_scalb(sx1, huge(0))) /= ieee_positive_inf) call abort
  if (ieee_class(ieee_scalb(-sx1, huge(0))) /= ieee_negative_inf) call abort
  if (ieee_class(ieee_scalb(sx1, -huge(0))) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_scalb(-sx1, -huge(0))) /= ieee_negative_zero) call abort

  sx1 = ieee_value(sx1, ieee_quiet_nan)
  if (ieee_class(ieee_scalb(sx1, 1)) /= ieee_quiet_nan) call abort
  sx1 = ieee_value(sx1, ieee_positive_inf)
  if (ieee_class(ieee_scalb(sx1, -42)) /= ieee_positive_inf) call abort
  sx1 = ieee_value(sx1, ieee_negative_inf)
  if (ieee_class(ieee_scalb(sx1, -42)) /= ieee_negative_inf) call abort

  dx1 = 1
  if (ieee_scalb(dx1, 2) /= 4.d0) call abort
  if (ieee_scalb(-dx1, 2) /= -4.d0) call abort
  if (ieee_scalb(dx1, -2) /= 1/4.d0) call abort
  if (ieee_scalb(-dx1, -2) /= -1/4.d0) call abort
  if (ieee_class(ieee_scalb(dx1, huge(0))) /= ieee_positive_inf) call abort
  if (ieee_class(ieee_scalb(-dx1, huge(0))) /= ieee_negative_inf) call abort
  if (ieee_class(ieee_scalb(dx1, -huge(0))) /= ieee_positive_zero) call abort
  if (ieee_class(ieee_scalb(-dx1, -huge(0))) /= ieee_negative_zero) call abort

  dx1 = ieee_value(dx1, ieee_quiet_nan)
  if (ieee_class(ieee_scalb(dx1, 1)) /= ieee_quiet_nan) call abort
  dx1 = ieee_value(dx1, ieee_positive_inf)
  if (ieee_class(ieee_scalb(dx1, -42)) /= ieee_positive_inf) call abort
  dx1 = ieee_value(dx1, ieee_negative_inf)
  if (ieee_class(ieee_scalb(dx1, -42)) /= ieee_negative_inf) call abort

contains

  subroutine check_equal_float (x, y)
    real, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x /= y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal_float (x, y)
    real, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

  subroutine check_not_equal_double (x, y)
    double precision, intent(in) :: x, y
    if (x == y) then
      print *, x, y
      call abort
    end if
  end subroutine

end
