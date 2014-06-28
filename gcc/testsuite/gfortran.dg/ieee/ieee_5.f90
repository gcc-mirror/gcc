! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  logical mode

  ! Test IEEE_SET_UNDERFLOW_MODE, IEEE_GET_UNDERFLOW_MODE,
  ! and IEEE_SUPPORT_UNDERFLOW_CONTROL
  !
  ! We don't have any targets where this is supported yet, so
  ! we just check these subroutines are present.

  if (ieee_support_underflow_control() &
      .or. ieee_support_underflow_control(0.)) then

    call ieee_get_underflow_mode(mode)
    call ieee_set_underflow_mode(.false.)
    call ieee_set_underflow_mode(.true.)
    call ieee_set_underflow_mode(mode)

  end if

  if (ieee_support_underflow_control() &
      .or. ieee_support_underflow_control(0.d0)) then

    call ieee_get_underflow_mode(mode)
    call ieee_set_underflow_mode(.false.)
    call ieee_set_underflow_mode(.true.)
    call ieee_set_underflow_mode(mode)

  end if

end
