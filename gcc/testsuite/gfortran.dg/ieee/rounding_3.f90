! { dg-do run }

  ! Test IEEE_GET_ROUNDING_MODE and IEEE_SET_ROUNDING_MODE
  ! with a RADIX argument
  use, intrinsic :: ieee_arithmetic
  implicit none

  real :: sx1
  type(ieee_round_type) :: r

  if (ieee_support_rounding(ieee_up, sx1) .and. &
      ieee_support_rounding(ieee_down, sx1)) then

    call ieee_set_rounding_mode(ieee_up)
    call ieee_get_rounding_mode(r)
    if (r /= ieee_up) stop 1

    call ieee_set_rounding_mode(ieee_down, radix=2)
    call ieee_get_rounding_mode(r, radix=2)
    if (r /= ieee_down) stop 2

    call ieee_set_rounding_mode(ieee_up, radix=10)
    call ieee_get_rounding_mode(r, radix=2)
    if (r /= ieee_down) stop 3
  end if

end
