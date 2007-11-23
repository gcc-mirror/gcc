! { dg-do run }
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
    call abort()
  ! 0++ > 0+
  if (nearest(nearest(0.0, 1.0), 1.0) &
      <= nearest(0.0, 1.0)) &
    call abort()
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0, 1.0), 1.0)) &
    call abort()
  ! 0+- = 0
  if (nearest(nearest(0.0, 1.0), -1.0) &
      /= 0.0) &
    call abort()
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0) &
      /= nearest(0.0, 1.0)) &
    call abort()
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0) &
    call abort()

  ! 0- < 0
  if (nearest(0.0, -1.0) &
      >= 0.0) &
    call abort()
  ! 0-- < 0+
  if (nearest(nearest(0.0, -1.0), -1.0) &
      >= nearest(0.0, -1.0)) &
    call abort()
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0, -1.0), -1.0)) &
    call abort()
  ! 0-+ = 0
  if (nearest(nearest(0.0, -1.0), 1.0) &
      /= 0.0) &
    call abort()
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0) &
      /= nearest(0.0, -1.0)) &
    call abort()
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0) &
    call abort()

  ! 42++ > 42+
  if (nearest(nearest(42.0, 1.0), 1.0) &
      <= nearest(42.0, 1.0)) &
    call abort()
  ! 42-- < 42-
  if (nearest(nearest(42.0, -1.0), -1.0) &
      >= nearest(42.0, -1.0)) &
    call abort()
  ! 42-+ = 42
  if (nearest(nearest(42.0, -1.0), 1.0) &
      /= 42.0) &
    call abort()
  ! 42+- = 42
  if (nearest(nearest(42.0, 1.0), -1.0) &
      /= 42.0) &
    call abort()

! Double precision

  ! 0+ > 0
  if (nearest(0.0d0, 1.0) &
      <= 0.0d0) &
    call abort()
  ! 0++ > 0+
  if (nearest(nearest(0.0d0, 1.0), 1.0) &
      <= nearest(0.0d0, 1.0)) &
    call abort()
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0d0, 1.0), 1.0)) &
    call abort()
  ! 0+- = 0
  if (nearest(nearest(0.0d0, 1.0), -1.0) &
      /= 0.0d0) &
    call abort()
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0) &
      /= nearest(0.0d0, 1.0)) &
    call abort()
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0d0) &
    call abort()

  ! 0- < 0
  if (nearest(0.0d0, -1.0) &
      >= 0.0d0) &
    call abort()
  ! 0-- < 0+
  if (nearest(nearest(0.0d0, -1.0), -1.0) &
      >= nearest(0.0d0, -1.0)) &
    call abort()
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0d0, -1.0), -1.0)) &
    call abort()
  ! 0-+ = 0
  if (nearest(nearest(0.0d0, -1.0), 1.0) &
      /= 0.0d0) &
    call abort()
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0) &
      /= nearest(0.0d0, -1.0)) &
    call abort()
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0d0) &
    call abort()

  ! 42++ > 42+
  if (nearest(nearest(42.0d0, 1.0), 1.0) &
      <= nearest(42.0d0, 1.0)) &
    call abort()
  ! 42-- < 42-
  if (nearest(nearest(42.0d0, -1.0), -1.0) &
      >= nearest(42.0d0, -1.0)) &
    call abort()
  ! 42-+ = 42
  if (nearest(nearest(42.0d0, -1.0), 1.0) &
      /= 42.0d0) &
    call abort()
  ! 42+- = 42
  if (nearest(nearest(42.0d0, 1.0), -1.0) &
      /= 42.0d0) &
    call abort()
end program test
