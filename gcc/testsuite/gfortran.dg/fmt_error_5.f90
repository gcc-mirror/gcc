! { dg-do run }
! { dg-shouldfail "runtime error" }

! PR fortran/29835
! Check for improved format error messages with correct locus and more detailed
! "unexpected element" messages.

! Now with runtime supplied format strings
SUBROUTINE format_runtime (fmtstr)
  IMPLICIT NONE
  CHARACTER(len=*) :: fmtstr
  INTEGER :: x

  PRINT fmtstr, x
END SUBROUTINE format_runtime

PROGRAM main
  IMPLICIT NONE
  CALL format_runtime ('(Q)')
END PROGRAM main

! { dg-output "Unexpected element 'Q'.*(\r*\n+)\\(Q\\)(\r*\n+) \\^" }
