! { dg-do run }
! Fixed by the patch for PRs 60357 and 61275
!
! Contributed by Stefan Mauerberger  <stefan.mauerberger@gmail.com>
!
PROGRAM main
  IMPLICIT NONE
  TYPE :: test_typ
    REAL, ALLOCATABLE :: a
  END TYPE
  TYPE(test_typ) :: my_test_typ
  my_test_typ = test_typ (a = 1.0)
  if (abs (my_test_typ%a - 1.0) .gt. 1e-6) STOP 1
END PROGRAM main
