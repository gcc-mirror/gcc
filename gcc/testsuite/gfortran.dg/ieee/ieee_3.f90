! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  integer, parameter :: s = kind(sx1), d = kind(dx1)
  type(ieee_round_type) :: mode

  ! Test IEEE_IS_FINITE

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_finite(0.2_s)) call abort
    if (.not. ieee_is_finite(-0.2_s)) call abort
    if (.not. ieee_is_finite(0._s)) call abort
    if (.not. ieee_is_finite(-0._s)) call abort
    if (.not. ieee_is_finite(tiny(0._s))) call abort
    if (.not. ieee_is_finite(tiny(0._s)/100)) call abort
    if (.not. ieee_is_finite(huge(0._s))) call abort
    if (.not. ieee_is_finite(-huge(0._s))) call abort
    sx1 = huge(sx1)
    if (ieee_is_finite(2*sx1)) call abort
    if (ieee_is_finite(2*(-sx1))) call abort
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_finite(sx1)) call abort
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_finite(0.2_d)) call abort
    if (.not. ieee_is_finite(-0.2_d)) call abort
    if (.not. ieee_is_finite(0._d)) call abort
    if (.not. ieee_is_finite(-0._d)) call abort
    if (.not. ieee_is_finite(tiny(0._d))) call abort
    if (.not. ieee_is_finite(tiny(0._d)/100)) call abort
    if (.not. ieee_is_finite(huge(0._d))) call abort
    if (.not. ieee_is_finite(-huge(0._d))) call abort
    dx1 = huge(dx1)
    if (ieee_is_finite(2*dx1)) call abort
    if (ieee_is_finite(2*(-dx1))) call abort
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_finite(dx1)) call abort
  end if

  ! Test IEEE_IS_NAN

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_nan(0.2_s)) call abort
    if (ieee_is_nan(-0.2_s)) call abort
    if (ieee_is_nan(0._s)) call abort
    if (ieee_is_nan(-0._s)) call abort
    if (ieee_is_nan(tiny(0._s))) call abort
    if (ieee_is_nan(tiny(0._s)/100)) call abort
    if (ieee_is_nan(huge(0._s))) call abort
    if (ieee_is_nan(-huge(0._s))) call abort
    sx1 = huge(sx1)
    if (ieee_is_nan(2*sx1)) call abort
    if (ieee_is_nan(2*(-sx1))) call abort
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(sx1)) call abort
    sx1 = -1
    if (.not. ieee_is_nan(sqrt(sx1))) call abort
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_nan(0.2_d)) call abort
    if (ieee_is_nan(-0.2_d)) call abort
    if (ieee_is_nan(0._d)) call abort
    if (ieee_is_nan(-0._d)) call abort
    if (ieee_is_nan(tiny(0._d))) call abort
    if (ieee_is_nan(tiny(0._d)/100)) call abort
    if (ieee_is_nan(huge(0._d))) call abort
    if (ieee_is_nan(-huge(0._d))) call abort
    dx1 = huge(dx1)
    if (ieee_is_nan(2*dx1)) call abort
    if (ieee_is_nan(2*(-dx1))) call abort
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(dx1)) call abort
    dx1 = -1
    if (.not. ieee_is_nan(sqrt(dx1))) call abort
  end if

  ! IEEE_IS_NEGATIVE

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_negative(0.2_s)) call abort
    if (.not. ieee_is_negative(-0.2_s)) call abort
    if (ieee_is_negative(0._s)) call abort
    if (.not. ieee_is_negative(-0._s)) call abort
    if (ieee_is_negative(tiny(0._s))) call abort
    if (ieee_is_negative(tiny(0._s)/100)) call abort
    if (.not. ieee_is_negative(-tiny(0._s))) call abort
    if (.not. ieee_is_negative(-tiny(0._s)/100)) call abort
    if (ieee_is_negative(huge(0._s))) call abort
    if (.not. ieee_is_negative(-huge(0._s))) call abort
    sx1 = huge(sx1)
    if (ieee_is_negative(2*sx1)) call abort
    if (.not. ieee_is_negative(2*(-sx1))) call abort
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_negative(sx1)) call abort
    sx1 = -1
    if (ieee_is_negative(sqrt(sx1))) call abort
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_negative(0.2_d)) call abort
    if (.not. ieee_is_negative(-0.2_d)) call abort
    if (ieee_is_negative(0._d)) call abort
    if (.not. ieee_is_negative(-0._d)) call abort
    if (ieee_is_negative(tiny(0._d))) call abort
    if (ieee_is_negative(tiny(0._d)/100)) call abort
    if (.not. ieee_is_negative(-tiny(0._d))) call abort
    if (.not. ieee_is_negative(-tiny(0._d)/100)) call abort
    if (ieee_is_negative(huge(0._d))) call abort
    if (.not. ieee_is_negative(-huge(0._d))) call abort
    dx1 = huge(dx1)
    if (ieee_is_negative(2*dx1)) call abort
    if (.not. ieee_is_negative(2*(-dx1))) call abort
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_negative(dx1)) call abort
    dx1 = -1
    if (ieee_is_negative(sqrt(dx1))) call abort
  end if

  ! Test IEEE_IS_NORMAL

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_normal(0.2_s)) call abort
    if (.not. ieee_is_normal(-0.2_s)) call abort
    if (.not. ieee_is_normal(0._s)) call abort
    if (.not. ieee_is_normal(-0._s)) call abort
    if (.not. ieee_is_normal(tiny(0._s))) call abort
    if (ieee_is_normal(tiny(0._s)/100)) call abort
    if (.not. ieee_is_normal(-tiny(0._s))) call abort
    if (ieee_is_normal(-tiny(0._s)/100)) call abort
    if (.not. ieee_is_normal(huge(0._s))) call abort
    if (.not. ieee_is_normal(-huge(0._s))) call abort
    sx1 = huge(sx1)
    if (ieee_is_normal(2*sx1)) call abort
    if (ieee_is_normal(2*(-sx1))) call abort
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_normal(sx1)) call abort
    sx1 = -1
    if (ieee_is_normal(sqrt(sx1))) call abort
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_normal(0.2_d)) call abort
    if (.not. ieee_is_normal(-0.2_d)) call abort
    if (.not. ieee_is_normal(0._d)) call abort
    if (.not. ieee_is_normal(-0._d)) call abort
    if (.not. ieee_is_normal(tiny(0._d))) call abort
    if (ieee_is_normal(tiny(0._d)/100)) call abort
    if (.not. ieee_is_normal(-tiny(0._d))) call abort
    if (ieee_is_normal(-tiny(0._d)/100)) call abort
    if (.not. ieee_is_normal(huge(0._d))) call abort
    if (.not. ieee_is_normal(-huge(0._d))) call abort
    dx1 = huge(dx1)
    if (ieee_is_normal(2*dx1)) call abort
    if (ieee_is_normal(2*(-dx1))) call abort
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_normal(dx1)) call abort
    dx1 = -1
    if (ieee_is_normal(sqrt(dx1))) call abort
  end if

end
