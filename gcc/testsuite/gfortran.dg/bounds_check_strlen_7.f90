! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Character length mismatch" }

! PR fortran/37746
! Test bounds-checking for string length of dummy arguments.

MODULE m
CONTAINS

  SUBROUTINE test (opt)
    IMPLICIT NONE
    CHARACTER(len=5), OPTIONAL :: opt
  END SUBROUTINE test

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  CALL test ('') ! 0 length, but not absent argument.
END PROGRAM main

! { dg-output "shorter than the declared one for dummy argument 'opt' \\(0/5\\)" }
! { dg-final { cleanup-modules "m" } }
