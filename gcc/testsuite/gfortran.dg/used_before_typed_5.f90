! { dg-do compile }
! { dg-options "-pedantic -std=f95" }

! Check that DIMENSION/POINTER/ALLOCATABLE/INTENT statements *do* allow
! symbols to be typed later.

SUBROUTINE test (a)
  IMPLICIT REAL (a-z)

  ! Those should *not* IMPLICIT-type the symbols:
  INTENT(IN) :: a
  DIMENSION :: b(:)
  POINTER :: c
  ALLOCATABLE :: b

  ! So this is ok:
  INTEGER :: a, b, c

END SUBROUTINE test
