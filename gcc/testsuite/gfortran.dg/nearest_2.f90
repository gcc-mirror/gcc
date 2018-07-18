! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
!
! PR fortran/34192
!
! Test compile-time implementation of NEAREST
!
program test
  implicit none

! Single precision

  ! 0+ > 0
  if (nearest(0.0, 1.0) &
      <= 0.0) &
    STOP 1
  ! 0++ > 0+
  if (nearest(nearest(0.0, 1.0), 1.0) &
      <= nearest(0.0, 1.0)) &
    STOP 2
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0, 1.0), 1.0)) &
    STOP 3
  ! 0+- = 0
  if (nearest(nearest(0.0, 1.0), -1.0) &
      /= 0.0) &
    STOP 4
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0) &
      /= nearest(0.0, 1.0)) &
    STOP 5
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0) &
    STOP 6

  ! 0- < 0
  if (nearest(0.0, -1.0) &
      >= 0.0) &
    STOP 7
  ! 0-- < 0+
  if (nearest(nearest(0.0, -1.0), -1.0) &
      >= nearest(0.0, -1.0)) &
    STOP 8
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0, -1.0), -1.0)) &
    STOP 9
  ! 0-+ = 0
  if (nearest(nearest(0.0, -1.0), 1.0) &
      /= 0.0) &
    STOP 10
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0) &
      /= nearest(0.0, -1.0)) &
    STOP 11
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0) &
    STOP 12

  ! 42++ > 42+
  if (nearest(nearest(42.0, 1.0), 1.0) &
      <= nearest(42.0, 1.0)) &
    STOP 13
  ! 42-- < 42-
  if (nearest(nearest(42.0, -1.0), -1.0) &
      >= nearest(42.0, -1.0)) &
    STOP 14
  ! 42-+ = 42
  if (nearest(nearest(42.0, -1.0), 1.0) &
      /= 42.0) &
    STOP 15
  ! 42+- = 42
  if (nearest(nearest(42.0, 1.0), -1.0) &
      /= 42.0) &
    STOP 16

  ! INF+ = INF
  if (nearest(1.0/0.0, 1.0) /= 1.0/0.0) STOP 17
  ! -INF- = -INF
  if (nearest(-1.0/0.0, -1.0) /= -1.0/0.0) STOP 18
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0d0/0.0,  1.0))) STOP 19
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0d0/0.0, -1.0))) STOP 20

! Double precision

  ! 0+ > 0
  if (nearest(0.0d0, 1.0) &
      <= 0.0d0) &
    STOP 21
  ! 0++ > 0+
  if (nearest(nearest(0.0d0, 1.0), 1.0) &
      <= nearest(0.0d0, 1.0)) &
    STOP 22
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0d0, 1.0), 1.0)) &
    STOP 23
  ! 0+- = 0
  if (nearest(nearest(0.0d0, 1.0), -1.0) &
      /= 0.0d0) &
    STOP 24
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0) &
      /= nearest(0.0d0, 1.0)) &
    STOP 25
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0d0) &
    STOP 26

  ! 0- < 0
  if (nearest(0.0d0, -1.0) &
      >= 0.0d0) &
    STOP 27
  ! 0-- < 0+
  if (nearest(nearest(0.0d0, -1.0), -1.0) &
      >= nearest(0.0d0, -1.0)) &
    STOP 28
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0d0, -1.0), -1.0)) &
    STOP 29
  ! 0-+ = 0
  if (nearest(nearest(0.0d0, -1.0), 1.0) &
      /= 0.0d0) &
    STOP 30
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0) &
      /= nearest(0.0d0, -1.0)) &
    STOP 31
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0d0) &
    STOP 32

  ! 42++ > 42+
  if (nearest(nearest(42.0d0, 1.0), 1.0) &
      <= nearest(42.0d0, 1.0)) &
    STOP 33
  ! 42-- < 42-
  if (nearest(nearest(42.0d0, -1.0), -1.0) &
      >= nearest(42.0d0, -1.0)) &
    STOP 34
  ! 42-+ = 42
  if (nearest(nearest(42.0d0, -1.0), 1.0) &
      /= 42.0d0) &
    STOP 35
  ! 42+- = 42
  if (nearest(nearest(42.0d0, 1.0), -1.0) &
      /= 42.0d0) &
    STOP 36

  ! INF+ = INF
  if (nearest(1.0d0/0.0d0, 1.0) /= 1.0d0/0.0d0) STOP 37
  ! -INF- = -INF
  if (nearest(-1.0d0/0.0d0, -1.0) /= -1.0d0/0.0d0) STOP 38
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0d0/0.0,  1.0))) STOP 39
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0d0/0.0, -1.0))) STOP 40
end program test
