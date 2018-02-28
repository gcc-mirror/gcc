! { dg-do run }
! { dg-additional-options "-ffree-line-length-none" }
! { dg-additional-options "-mfp-trap-mode=sui" { target alpha*-*-* } }
!
! Use dg-additional-options rather than dg-options to avoid overwriting the
! default IEEE options which are passed by ieee.exp and necessary.

  use ieee_features, only : ieee_datatype, ieee_denormal, ieee_divide, &
      ieee_halting, ieee_inexact_flag, ieee_inf, ieee_invalid_flag, &
      ieee_nan, ieee_rounding, ieee_sqrt, ieee_underflow_flag
  use ieee_exceptions

  implicit none

  type(ieee_flag_type), parameter :: x(5) = &
    [ IEEE_INVALID, IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO, &
      IEEE_UNDERFLOW, IEEE_INEXACT ]
  logical :: l(5) = .false.
  character(len=5) :: s

#define FLAGS_STRING(S) \
  call ieee_get_flag(x, l) ; \
  write(S,"(5(A1))") merge(["I","O","Z","U","P"],[" "," "," "," "," "],l)

#define CHECK_FLAGS(expected) \
  FLAGS_STRING(s) ; \
  if (s /= expected) then ; \
    write (*,"(A,I0,A,A)") "Flags at line ", __LINE__, ": ", s ; \
    STOP 1; \
  end if ; \
  call check_flag_sub

  real, volatile :: sx
  double precision, volatile :: dx

  ! This file tests IEEE_SET_FLAG and IEEE_GET_FLAG

  !!!! IEEE float

  ! Initial flags are all off
  CHECK_FLAGS("     ")

  ! Check we can clear them
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

  ! Raise invalid, then clear
  sx = -1
  sx = sqrt(sx)
  CHECK_FLAGS("I    ")
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

  ! Raise overflow and precision
  sx = huge(sx)
  CHECK_FLAGS("     ")
  sx = sx*sx
  CHECK_FLAGS(" O  P")

  ! Also raise divide-by-zero
  sx = 0
  sx = 1 / sx
  CHECK_FLAGS(" OZ P")

  ! Clear them
  call ieee_set_flag([ieee_overflow,ieee_inexact,&
                      ieee_divide_by_zero],[.false.,.false.,.true.])
  CHECK_FLAGS("  Z  ")
  call ieee_set_flag(ieee_divide_by_zero, .false.)
  CHECK_FLAGS("     ")

  ! Raise underflow
  sx = tiny(sx)
  CHECK_FLAGS("     ")
  sx = sx / 10
  CHECK_FLAGS("   UP")

  ! Raise everything
  call ieee_set_flag(ieee_all, .true.)
  CHECK_FLAGS("IOZUP")

  ! And clear
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

  !!!! IEEE double

  ! Initial flags are all off
  CHECK_FLAGS("     ")

  ! Check we can clear them
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

  ! Raise invalid, then clear
  dx = -1
  dx = sqrt(dx)
  CHECK_FLAGS("I    ")
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

  ! Raise overflow and precision
  dx = huge(dx)
  CHECK_FLAGS("     ")
  dx = dx*dx
  CHECK_FLAGS(" O  P")

  ! Also raise divide-by-zero
  dx = 0
  dx = 1 / dx
  CHECK_FLAGS(" OZ P")

  ! Clear them
  call ieee_set_flag([ieee_overflow,ieee_inexact,&
                      ieee_divide_by_zero],[.false.,.false.,.true.])
  CHECK_FLAGS("  Z  ")
  call ieee_set_flag(ieee_divide_by_zero, .false.)
  CHECK_FLAGS("     ")

  ! Raise underflow
  dx = tiny(dx)
  CHECK_FLAGS("     ")
  dx = dx / 10
  CHECK_FLAGS("   UP")

  ! Raise everything
  call ieee_set_flag(ieee_all, .true.)
  CHECK_FLAGS("IOZUP")

  ! And clear
  call ieee_set_flag(ieee_all, .false.)
  CHECK_FLAGS("     ")

contains

  subroutine check_flag_sub
    use ieee_exceptions
    logical :: l(5) = .false.
    type(ieee_flag_type), parameter :: x(5) = &
      [ IEEE_INVALID, IEEE_OVERFLOW, IEEE_DIVIDE_BY_ZERO, &
        IEEE_UNDERFLOW, IEEE_INEXACT ]
    call ieee_get_flag(x, l)

    if (any(l)) then
      print *, "Flags not cleared in subroutine"
      STOP 2
    end if
  end subroutine

end
