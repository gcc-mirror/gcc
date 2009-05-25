! { dg-do run }
!
! PR 40176:  Fortran 2003: Procedure pointers with array return value
!
! Original test case by Barron Bichon <barron.bichon@swri.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

PROGRAM test_prog

  PROCEDURE(triple), POINTER :: f

  f => triple
  if (sum(f(2.,4.)-triple(2.,4.))>1E-3) call abort()

CONTAINS

  FUNCTION triple(a,b) RESULT(tre)
    REAL, INTENT(in) :: a, b
    REAL :: tre(2)
    tre(1) = 3.*a
    tre(2) = 3.*b
  END FUNCTION triple

END PROGRAM test_prog

