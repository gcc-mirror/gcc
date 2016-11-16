! { dg-do run }
!
! This test will fail on older x86_64 glibc (< 2.20), due to this bug:
! https://sourceware.org/bugzilla/show_bug.cgi?id=16198
! We usually won't see it anyway, because on such systems x86_64 assembly
! (libgfortran/config/fpu-387.h) is used.
!
  use :: ieee_arithmetic
  implicit none

  type(ieee_status_type) :: s1, s2
  logical :: flags(5), halt(5), haltworks
  type(ieee_round_type) :: mode
  real :: x

  ! Test IEEE_GET_STATUS and IEEE_SET_STATUS

  call ieee_set_flag(ieee_all, .false.)
  call ieee_set_rounding_mode(ieee_down)
  call ieee_set_halting_mode(ieee_all, .false.)
  haltworks = ieee_support_halting(ieee_overflow)

  call ieee_get_status(s1)
  call ieee_set_status(s1)

  call ieee_get_flag(ieee_all, flags)
  if (any(flags)) call abort
  call ieee_get_rounding_mode(mode)
  if (mode /= ieee_down) call abort
  call ieee_get_halting_mode(ieee_all, halt)
  if (any(halt)) call abort

  call ieee_set_rounding_mode(ieee_to_zero)
  call ieee_set_flag(ieee_underflow, .true.)
  call ieee_set_halting_mode(ieee_overflow, .true.)
  x = -1
  x = sqrt(x)
  if (.not. ieee_is_nan(x)) call abort

  call ieee_get_status(s2)

  call ieee_get_flag(ieee_all, flags)
  if (.not. (all(flags .eqv. [.false.,.false.,.true.,.true.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.true.,.true.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.true.]))) call abort
  call ieee_get_rounding_mode(mode)
  if (mode /= ieee_to_zero) call abort
  call ieee_get_halting_mode(ieee_all, halt)
  if ((haltworks .and. .not. halt(1)) .or. any(halt(2:))) call abort

  call ieee_set_status(s2)

  call ieee_get_flag(ieee_all, flags)
  if (.not. (all(flags .eqv. [.false.,.false.,.true.,.true.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.true.,.true.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.true.]))) call abort
  call ieee_get_rounding_mode(mode)
  if (mode /= ieee_to_zero) call abort
  call ieee_get_halting_mode(ieee_all, halt)
  if ((haltworks .and. .not. halt(1)) .or. any(halt(2:))) call abort

  call ieee_set_status(s1)

  call ieee_get_flag(ieee_all, flags)
  if (any(flags)) call abort
  call ieee_get_rounding_mode(mode)
  if (mode /= ieee_down) call abort
  call ieee_get_halting_mode(ieee_all, halt)
  if (any(halt)) call abort

  call ieee_set_status(s2)

  call ieee_get_flag(ieee_all, flags)
  if (.not. (all(flags .eqv. [.false.,.false.,.true.,.true.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.true.,.true.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.false.]) &
             .or. all(flags .eqv. [.false.,.false.,.true.,.false.,.true.]))) call abort
  call ieee_get_rounding_mode(mode)
  if (mode /= ieee_to_zero) call abort
  call ieee_get_halting_mode(ieee_all, halt)
  if ((haltworks .and. .not. halt(1)) .or. any(halt(2:))) call abort

end
