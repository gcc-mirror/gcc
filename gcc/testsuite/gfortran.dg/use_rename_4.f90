! { dg-do run }

! PR fortran/37193
! Check fix for problem with re-using the same symbol both renamed and
! plain.

MODULE m
  IMPLICIT NONE
  INTEGER :: i
END MODULE m

PROGRAM main
  USE m, ONLY: i, j => i
  IMPLICIT NONE

  i = 4
  j = 5

  IF (i /= j) THEN
    CALL abort ()
  END IF
END PROGRAM main
