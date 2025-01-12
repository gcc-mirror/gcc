! { dg-do run }
! { dg-additional-options "-funsigned" }
!
! PR fortran/115788 - OUT_OF_RANGE

program p
  use, intrinsic :: ieee_arithmetic
  implicit none
  real    :: inf, nan
  real    :: r = 0.
  logical :: t = .true., f = .false.
  double precision :: dinf, dnan

  inf = ieee_value (inf, ieee_positive_inf)

  if (.not. OUT_OF_RANGE (inf, 0))          stop 1
  if (.not. OUT_OF_RANGE (inf, 0, f))       stop 2
  if (.not. OUT_OF_RANGE (inf, 0, t))       stop 3
  if (.not. OUT_OF_RANGE (inf, 0, .false.)) stop 4
  if (.not. OUT_OF_RANGE (inf, 0, .true.))  stop 5

  if (.not. OUT_OF_RANGE (inf, 0U))          stop 6
  if (.not. OUT_OF_RANGE (inf, 0U, f))       stop 7
  if (.not. OUT_OF_RANGE (inf, 0U, t))       stop 8
  if (.not. OUT_OF_RANGE (inf, 0U, .false.)) stop 9
  if (.not. OUT_OF_RANGE (inf, 0U, .true.))  stop 10

  if (OUT_OF_RANGE (inf, r)) stop 11

  dinf = ieee_value (dinf, ieee_positive_inf)

  if (OUT_OF_RANGE (inf, dinf))  stop 12
  if (OUT_OF_RANGE (dinf, inf))  stop 13
  if (OUT_OF_RANGE (dinf, dinf)) stop 14

  call check_nan ()

contains

  subroutine check_nan ()
    if (.not. ieee_support_nan (nan)) return
    nan = ieee_value (nan, ieee_quiet_nan)

    if (.not. OUT_OF_RANGE (nan, 0))          stop 15
    if (.not. OUT_OF_RANGE (nan, 0, f))       stop 16
    if (.not. OUT_OF_RANGE (nan, 0, t))       stop 17
    if (.not. OUT_OF_RANGE (nan, 0, .false.)) stop 18
    if (.not. OUT_OF_RANGE (nan, 0, .true.))  stop 19

    if (.not. OUT_OF_RANGE (nan, 0U))          stop 20
    if (.not. OUT_OF_RANGE (nan, 0U, f))       stop 21
    if (.not. OUT_OF_RANGE (nan, 0U, t))       stop 22
    if (.not. OUT_OF_RANGE (nan, 0U, .false.)) stop 23
    if (.not. OUT_OF_RANGE (nan, 0U, .true.))  stop 24

    if (OUT_OF_RANGE (nan, r)) stop 25

    if (.not. ieee_support_nan(dnan)) return
    dnan = ieee_value(dnan, ieee_quiet_nan)

    if (OUT_OF_RANGE (nan, dnan)) stop 26
    if (OUT_OF_RANGE (dnan, nan)) stop 27
  end subroutine check_nan

end
