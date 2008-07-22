! { dg-do run }
! { dg-shouldfail "runtime error" }

! PR fortran/29835
! Check for improved format error messages with correct locus and more detailed
! "unexpected element" messages.

! Now with runtime supplied format strings
SUBROUTINE format_runtime (fmtstr)
  IMPLICIT NONE
  CHARACTER(len=*) :: fmtstr
  CHARACTER(len=32), PARAMETER :: str = "hello"

  PRINT fmtstr, str, str, str
END SUBROUTINE format_runtime

PROGRAM main
  IMPLICIT NONE
  CALL format_runtime ('(A, Q, A)')
END PROGRAM main

! { dg-output "Unexpected element 'Q'.*(\n|\r\n|\r)\\(A, Q, A\\)(\n|\r\n|\r)    \\^" }
