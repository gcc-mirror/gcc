! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Character length mismatch" }

! PR fortran/37746
! Test bounds-checking for string length of dummy arguments.

MODULE m

CONTAINS

  SUBROUTINE test (str, n)
    IMPLICIT NONE
    INTEGER :: n
    CHARACTER(len=n) :: str
  END SUBROUTINE test

  SUBROUTINE test2 (str)
    IMPLICIT NONE
    CHARACTER(len=*) :: str
    CALL test (str, 5) ! Expected length of str is 5.
  END SUBROUTINE test2

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  CALL test2 ('abc') ! String is too short.
END PROGRAM main

! { dg-output "shorter than the declared one for dummy argument 'str' \\(3/5\\)" }
