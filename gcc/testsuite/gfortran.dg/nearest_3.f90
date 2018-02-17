! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
! PR fortran/34209
!
! Test run-time implementation of NEAREST
!
program test
  implicit none
  real(4), volatile :: r4
  real(8), volatile :: r8

! Single precision with single-precision sign

  r4 = 0.0_4
  ! 0+ > 0
  if (nearest(r4, 1.0) &
      <= r4) &
    STOP 1
  ! 0++ > 0+
  if (nearest(nearest(r4, 1.0), 1.0) &
      <= nearest(r4, 1.0)) &
    STOP 2
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r4, 1.0), 1.0), 1.0) &
      <= nearest(nearest(r4, 1.0), 1.0)) &
    STOP 3
  ! 0+- = 0
  if (nearest(nearest(r4, 1.0), -1.0) &
      /= r4) &
    STOP 4
  ! 0++- = 0+
  if (nearest(nearest(nearest(r4, 1.0), 1.0), -1.0) &
      /= nearest(r4, 1.0)) &
    STOP 5
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r4, 1.0), 1.0), -1.0), -1.0) &
      /= r4) &
    STOP 6

  ! 0- < 0
  if (nearest(r4, -1.0) &
      >= r4) &
    STOP 7
  ! 0-- < 0+
  if (nearest(nearest(r4, -1.0), -1.0) &
      >= nearest(r4, -1.0)) &
    STOP 8
  ! 0--- < 0--
  if (nearest(nearest(nearest(r4, -1.0), -1.0), -1.0) &
      >= nearest(nearest(r4, -1.0), -1.0)) &
    STOP 9
  ! 0-+ = 0
  if (nearest(nearest(r4, -1.0), 1.0) &
      /= r4) &
    STOP 10
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r4, -1.0), -1.0), 1.0) &
      /= nearest(r4, -1.0)) &
    STOP 11
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r4, -1.0), -1.0), 1.0), 1.0) &
      /= r4) &
    STOP 12

  r4 = 42.0_4
  ! 42++ > 42+
  if (nearest(nearest(r4, 1.0), 1.0) &
      <= nearest(r4, 1.0)) &
    STOP 13
  ! 42-- < 42-
  if (nearest(nearest(r4, -1.0), -1.0) &
      >= nearest(r4, -1.0)) &
    STOP 14
  ! 42-+ = 42
  if (nearest(nearest(r4, -1.0), 1.0) &
      /= r4) &
    STOP 15
  ! 42+- = 42
  if (nearest(nearest(r4, 1.0), -1.0) &
      /= r4) &
    STOP 16

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0/r4, 1.0) /= 1.0/r4) STOP 17
  ! -INF- = -INF
  if (nearest(-1.0/r4, -1.0) /= -1.0/r4) STOP 18
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0))) STOP 19
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0))) STOP 20

! Double precision with single-precision sign

  r8 = 0.0_8
  ! 0+ > 0
  if (nearest(r8, 1.0) &
      <= r8) &
    STOP 21
  ! 0++ > 0+
  if (nearest(nearest(r8, 1.0), 1.0) &
      <= nearest(r8, 1.0)) &
    STOP 22
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r8, 1.0), 1.0), 1.0) &
      <= nearest(nearest(r8, 1.0), 1.0)) &
    STOP 23
  ! 0+- = 0
  if (nearest(nearest(r8, 1.0), -1.0) &
      /= r8) &
    STOP 24
  ! 0++- = 0+
  if (nearest(nearest(nearest(r8, 1.0), 1.0), -1.0) &
      /= nearest(r8, 1.0)) &
    STOP 25
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r8, 1.0), 1.0), -1.0), -1.0) &
      /= r8) &
    STOP 26

  ! 0- < 0
  if (nearest(r8, -1.0) &
      >= r8) &
    STOP 27
  ! 0-- < 0+
  if (nearest(nearest(r8, -1.0), -1.0) &
      >= nearest(r8, -1.0)) &
    STOP 28
  ! 0--- < 0--
  if (nearest(nearest(nearest(r8, -1.0), -1.0), -1.0) &
      >= nearest(nearest(r8, -1.0), -1.0)) &
    STOP 29
  ! 0-+ = 0
  if (nearest(nearest(r8, -1.0), 1.0) &
      /= r8) &
    STOP 30
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r8, -1.0), -1.0), 1.0) &
      /= nearest(r8, -1.0)) &
    STOP 31
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r8, -1.0), -1.0), 1.0), 1.0) &
      /= r8) &
    STOP 32

  r8 = 42.0_8
  ! 42++ > 42+
  if (nearest(nearest(r8, 1.0), 1.0) &
      <= nearest(r8, 1.0)) &
    STOP 33
  ! 42-- < 42-
  if (nearest(nearest(r8, -1.0), -1.0) &
      >= nearest(r8, -1.0)) &
    STOP 34
  ! 42-+ = 42
  if (nearest(nearest(r8, -1.0), 1.0) &
      /= r8) &
    STOP 35
  ! 42+- = 42
  if (nearest(nearest(r8, 1.0), -1.0) &
      /= r8) &
    STOP 36

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0/r4, 1.0) /= 1.0/r4) STOP 37
  ! -INF- = -INF
  if (nearest(-1.0/r4, -1.0) /= -1.0/r4) STOP 38
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0))) STOP 39
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0))) STOP 40


! Single precision with double-precision sign

  r4 = 0.0_4
  ! 0+ > 0
  if (nearest(r4, 1.0d0) &
      <= r4) &
    STOP 41
  ! 0++ > 0+
  if (nearest(nearest(r4, 1.0d0), 1.0d0) &
      <= nearest(r4, 1.0d0)) &
    STOP 42
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r4, 1.0d0), 1.0d0), 1.0d0) &
      <= nearest(nearest(r4, 1.0d0), 1.0d0)) &
    STOP 43
  ! 0+- = 0
  if (nearest(nearest(r4, 1.0d0), -1.0d0) &
      /= r4) &
    STOP 44
  ! 0++- = 0+
  if (nearest(nearest(nearest(r4, 1.0d0), 1.0d0), -1.0d0) &
      /= nearest(r4, 1.0d0)) &
    STOP 45
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r4, 1.0d0), 1.0d0), -1.0d0), -1.0d0) &
      /= r4) &
    STOP 46

  ! 0- < 0
  if (nearest(r4, -1.0d0) &
      >= r4) &
    STOP 47
  ! 0-- < 0+
  if (nearest(nearest(r4, -1.0d0), -1.0d0) &
      >= nearest(r4, -1.0d0)) &
    STOP 48
  ! 0--- < 0--
  if (nearest(nearest(nearest(r4, -1.0d0), -1.0d0), -1.0d0) &
      >= nearest(nearest(r4, -1.0d0), -1.0d0)) &
    STOP 49
  ! 0-+ = 0
  if (nearest(nearest(r4, -1.0d0), 1.0d0) &
      /= r4) &
    STOP 50
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r4, -1.0d0), -1.0d0), 1.0d0) &
      /= nearest(r4, -1.0d0)) &
    STOP 51
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r4, -1.0d0), -1.0d0), 1.0d0), 1.0d0) &
      /= r4) &
    STOP 52

  r4 = 42.0_4
  ! 42++ > 42+
  if (nearest(nearest(r4, 1.0d0), 1.0d0) &
      <= nearest(r4, 1.0d0)) &
    STOP 53
  ! 42-- < 42-
  if (nearest(nearest(r4, -1.0d0), -1.0d0) &
      >= nearest(r4, -1.0d0)) &
    STOP 54
  ! 42-+ = 42
  if (nearest(nearest(r4, -1.0d0), 1.0d0) &
      /= r4) &
    STOP 55
  ! 42+- = 42
  if (nearest(nearest(r4, 1.0d0), -1.0d0) &
      /= r4) &
    STOP 56

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0d0/r4, 1.0d0) /= 1.0d0/r4) STOP 57
  ! -INF- = -INF
  if (nearest(-1.0d0/r4, -1.0d0) /= -1.0d0/r4) STOP 58
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0d0))) STOP 59
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0d0))) STOP 60

! Double precision with double-precision sign

  r8 = 0.0_8
  ! 0+ > 0
  if (nearest(r8, 1.0d0) &
      <= r8) &
    STOP 61
  ! 0++ > 0+
  if (nearest(nearest(r8, 1.0d0), 1.0d0) &
      <= nearest(r8, 1.0d0)) &
    STOP 62
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r8, 1.0d0), 1.0d0), 1.0d0) &
      <= nearest(nearest(r8, 1.0d0), 1.0d0)) &
    STOP 63
  ! 0+- = 0
  if (nearest(nearest(r8, 1.0d0), -1.0d0) &
      /= r8) &
    STOP 64
  ! 0++- = 0+
  if (nearest(nearest(nearest(r8, 1.0d0), 1.0d0), -1.0d0) &
      /= nearest(r8, 1.0d0)) &
    STOP 65
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r8, 1.0d0), 1.0d0), -1.0d0), -1.0d0) &
      /= r8) &
    STOP 66

  ! 0- < 0
  if (nearest(r8, -1.0d0) &
      >= r8) &
    STOP 67
  ! 0-- < 0+
  if (nearest(nearest(r8, -1.0d0), -1.0d0) &
      >= nearest(r8, -1.0d0)) &
    STOP 68
  ! 0--- < 0--
  if (nearest(nearest(nearest(r8, -1.0d0), -1.0d0), -1.0d0) &
      >= nearest(nearest(r8, -1.0d0), -1.0d0)) &
    STOP 69
  ! 0-+ = 0
  if (nearest(nearest(r8, -1.0d0), 1.0d0) &
      /= r8) &
    STOP 70
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r8, -1.0d0), -1.0d0), 1.0d0) &
      /= nearest(r8, -1.0d0)) &
    STOP 71
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r8, -1.0d0), -1.0d0), 1.0d0), 1.0d0) &
      /= r8) &
    STOP 72

  r8 = 42.0_8
  ! 42++ > 42+
  if (nearest(nearest(r8, 1.0d0), 1.0d0) &
      <= nearest(r8, 1.0d0)) &
    STOP 73
  ! 42-- < 42-
  if (nearest(nearest(r8, -1.0d0), -1.0d0) &
      >= nearest(r8, -1.0d0)) &
    STOP 74
  ! 42-+ = 42
  if (nearest(nearest(r8, -1.0d0), 1.0d0) &
      /= r8) &
    STOP 75
  ! 42+- = 42
  if (nearest(nearest(r8, 1.0d0), -1.0d0) &
      /= r8) &
    STOP 76

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0d0/r4, 1.0d0) /= 1.0d0/r4) STOP 77
  ! -INF- = -INF
  if (nearest(-1.0d0/r4, -1.0d0) /= -1.0d0/r4) STOP 78
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0d0))) STOP 79
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0d0))) STOP 80

end program test
