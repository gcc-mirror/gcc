! { dg-do compile }

! PR fortran/37193
! Check that renamed symbols are not accessiable uner their target name.

MODULE m
  IMPLICIT NONE
  INTEGER :: i
END MODULE m

PROGRAM main
  USE m, ONLY: j => i
  IMPLICIT NONE

  i = 4 ! { dg-error "no IMPLICIT type" }
  j = 5
END PROGRAM main
