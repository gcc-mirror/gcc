! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Character length mismatch" }

! PR fortran/37746
! Test bounds-checking for string length of dummy arguments.

SUBROUTINE test (str)
  IMPLICIT NONE
  CHARACTER(len=5) :: str
END SUBROUTINE test

PROGRAM main
  IMPLICIT NONE
  CALL test ('abc') ! String is too short.
END PROGRAM main

! { dg-output "shorter than the declared one for dummy argument 'str' \\(3/5\\)" }
