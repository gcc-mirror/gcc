! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Character length mismatch" }

! PR fortran/37746
! Test bounds-checking for string length of dummy arguments.

MODULE m

CONTAINS

  SUBROUTINE test (str)
    IMPLICIT NONE
    CHARACTER(len=5), POINTER :: str
  END SUBROUTINE test

  SUBROUTINE test2 (n)
    IMPLICIT NONE
    INTEGER :: n
    CHARACTER(len=n), POINTER :: str
    CALL test (str)
  END SUBROUTINE test2

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  CALL test2 (7) ! Too long.
END PROGRAM main

! { dg-output "does not match the declared one for dummy argument 'str' \\(7/5\\)" }
! { dg-final { cleanup-modules "m" } }
