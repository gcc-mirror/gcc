! { dg-do run }

! PR fortran/37099
! Check for correct results when comparing array-section-substrings.

PROGRAM main
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: exprs(1) = (/ 'aa' /)

  CHARACTER(*), PARAMETER :: al1 = 'a';
  CHARACTER(len=LEN (al1)) :: al2 = al1;

  LOGICAL :: tmp(1), tmp2(1)

  tmp = (exprs(1:1)(1:1) == al1)
  tmp2 = (exprs(1:1)(1:1) == al2)

  PRINT '(L1)', tmp
  PRINT '(L1)', tmp2

  IF (.NOT. tmp(1) .OR. .NOT. tmp2(1)) THEN
    STOP 1
  END IF
END PROGRAM main
