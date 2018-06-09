! { dg-do run { xfail i?86-*-freebsd* } }

program test_underflow_control
  use ieee_arithmetic
  use iso_fortran_env

  ! kx and ky will be large real kinds, if supported, and single/double
  ! otherwise
  integer, parameter :: kx = &
    max(ieee_selected_real_kind(precision(0.d0) + 1), kind(0.))
  integer, parameter :: ky = &
    max(ieee_selected_real_kind(precision(0._kx) + 1), kind(0.d0))

  logical l
  real(kind=kx), volatile :: x
  real(kind=ky), volatile :: y

  if (ieee_support_underflow_control(x)) then

    x = tiny(x)
    call ieee_set_underflow_mode(.true.)
    x = x / 2000._kx
    if (x == 0) STOP 1
    call ieee_get_underflow_mode(l)
    if (.not. l) STOP 2

    x = tiny(x)
    call ieee_set_underflow_mode(.false.)
    x = x / 2000._kx
    if (x > 0) STOP 3
    call ieee_get_underflow_mode(l)
    if (l) STOP 4

  end if

  if (ieee_support_underflow_control(y)) then

    y = tiny(y)
    call ieee_set_underflow_mode(.true.)
    y = y / 2000._ky
    if (y == 0) STOP 5
    call ieee_get_underflow_mode(l)
    if (.not. l) STOP 6

    y = tiny(y)
    call ieee_set_underflow_mode(.false.)
    y = y / 2000._ky
    if (y > 0) STOP 7
    call ieee_get_underflow_mode(l)
    if (l) STOP 8

  end if

end program
