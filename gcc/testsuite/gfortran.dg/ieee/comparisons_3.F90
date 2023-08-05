! { dg-do run }
! { dg-additional-options "-ffree-line-length-none" }
program foo
  use ieee_arithmetic
  use iso_fortran_env
  implicit none

  ! This allows us to test REAL128 if it exists, and still compile
  ! on platforms were it is not present
  ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89639
  integer, parameter :: large = merge(real128, real64, real128 > 0)

  real, volatile :: rnan, rinf
  double precision, volatile :: dnan, dinf
  real(kind=large), volatile :: lnan, linf

  logical :: flag

  rinf = ieee_value(0., ieee_positive_inf)
  rnan = ieee_value(0., ieee_quiet_nan)

  dinf = ieee_value(0.d0, ieee_positive_inf)
  dnan = ieee_value(0.d0, ieee_quiet_nan)

  linf = ieee_value(0._large, ieee_positive_inf)
  lnan = ieee_value(0._large, ieee_quiet_nan)

#define CHECK_INVALID(expected) \
  call ieee_get_flag(ieee_invalid, flag) ; \
  if (flag .neqv. expected) then ; \
    write (*,*) "Check failed at ", __LINE__ ; \
    stop 1; \
  end if ; \
  call ieee_set_flag(ieee_invalid, .false.)

  !! REAL

  ! Signaling versions

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_eq (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_ne (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_le (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_lt (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_ge (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0., rnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_gt (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (rnan, rnan)) stop 15
  CHECK_INVALID(.true.)

  ! Quiet versions

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_lt (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0., 0.)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0., -0.)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0., rnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0., rinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (rnan, rnan)) stop 15
  CHECK_INVALID(.false.)

  !! DOUBLE PRECISION

  ! Signaling versions

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_eq (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_ne (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_le (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_lt (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_ge (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0.d0, dnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_gt (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (dnan, dnan)) stop 15
  CHECK_INVALID(.true.)

  ! Quiet versions

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_lt (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0.d0, 0.d0)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0.d0, -0.d0)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0.d0, dnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0.d0, dinf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (dnan, dnan)) stop 15
  CHECK_INVALID(.false.)

  !! LARGE KIND

  ! Signaling versions

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_eq (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_eq (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_eq (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_ne (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_ne (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ne (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_le (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_le (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_le (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (.not. ieee_signaling_lt (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_lt (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_signaling_ge (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_ge (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_ge (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (0._large, lnan)) stop 13
  CHECK_INVALID(.true.)
  if (ieee_signaling_gt (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_signaling_gt (lnan, lnan)) stop 15
  CHECK_INVALID(.true.)

  ! Quiet versions

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_eq (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_eq (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_ne (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ne (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_le (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_le (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_lt (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_lt (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (.not. ieee_quiet_ge (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_ge (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)

  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0._large, 0._large)) stop 11
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0._large, -0._large)) stop 12
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0._large, lnan)) stop 13
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (0._large, linf)) stop 14
  CHECK_INVALID(.false.)
  if (ieee_quiet_gt (lnan, lnan)) stop 15
  CHECK_INVALID(.false.)


end program foo
