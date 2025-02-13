  ! { dg-do compile }
MODULE m
  IMPLICIT NONE

CONTAINS

  SUBROUTINE test ()
    IMPLICIT NONE

    CALL bar (test2) ! { dg-error "Interface mismatch in dummy procedure" }
  END SUBROUTINE test

  INTEGER FUNCTION test2 () RESULT (x)
    IMPLICIT NONE

    CALL bar (test) ! { dg-error "Interface mismatch in dummy procedure" }
  END FUNCTION test2

END MODULE m

