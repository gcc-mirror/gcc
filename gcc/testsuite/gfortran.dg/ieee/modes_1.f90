! { dg-do run }
!
! Test IEEE_MODES_TYPE, IEEE_GET_MODES and IEEE_SET_MODES


! The symbols should be accessible from both IEEE_EXCEPTIONS
! and IEEE_ARITHMETIC.

subroutine test_1
  use ieee_exceptions, only : IEEE_GET_MODES, IEEE_SET_MODES
end subroutine

subroutine test_2
  use ieee_arithmetic, only : IEEE_GET_MODES, IEEE_SET_MODES
end subroutine

subroutine test_3
  use ieee_exceptions, only : IEEE_MODES_TYPE
end subroutine

subroutine test_4
  use ieee_arithmetic, only : IEEE_MODES_TYPE
end subroutine


! Check that the functions actually do the job

program foo
  use ieee_arithmetic
  implicit none

  type(ieee_modes_type) :: modes1, modes2
  type(ieee_round_type) :: rmode
  logical :: f

  ! Set some modes
  if (ieee_support_underflow_control()) then
    call ieee_set_underflow_mode(gradual=.false.)
  endif
  if (ieee_support_rounding(ieee_up)) then
    call ieee_set_rounding_mode(ieee_up)
  endif
  if (ieee_support_halting(ieee_overflow)) then
    call ieee_set_halting_mode(ieee_overflow, .true.)
  endif

  call ieee_get_modes(modes1)

  ! Change modes
  if (ieee_support_underflow_control()) then
    call ieee_set_underflow_mode(gradual=.true.)
  endif
  if (ieee_support_rounding(ieee_down)) then
    call ieee_set_rounding_mode(ieee_down)
  endif
  if (ieee_support_halting(ieee_overflow)) then
    call ieee_set_halting_mode(ieee_overflow, .false.)
  endif

  ! Save and restore the previous modes
  call ieee_get_modes(modes2)
  call ieee_set_modes(modes1)

  ! Check them
  if (ieee_support_underflow_control()) then
    call ieee_get_underflow_mode(f)
    if (f) stop 1
  endif
  if (ieee_support_rounding(ieee_down)) then
    call ieee_get_rounding_mode(rmode)
    if (rmode /= ieee_up) stop 2
  endif
  if (ieee_support_halting(ieee_overflow)) then
    call ieee_get_halting_mode(ieee_overflow, f)
    if (.not. f) stop 3
  endif

  ! Restore the second set of modes
  call ieee_set_modes(modes2)

  ! Check again
  if (ieee_support_underflow_control()) then
    call ieee_get_underflow_mode(f)
    if (.not. f) stop 4
  endif
  if (ieee_support_rounding(ieee_down)) then
    call ieee_get_rounding_mode(rmode)
    if (rmode /= ieee_down) stop 5
  endif
  if (ieee_support_halting(ieee_overflow)) then
    call ieee_get_halting_mode(ieee_overflow, f)
    if (f) stop 6
  endif

end program foo
