! { dg-do compile }
! { dg-options "-std=f2008" }

! Check for label mismatch errors with BLOCK statements.

PROGRAM main
  IMPLICIT NONE

  BLOCK 
  END BLOCK wrongname ! { dg-error "Syntax error" }

  myname: BLOCK
  END BLOCK wrongname ! { dg-error "Expected label 'myname'" }

  myname2: BLOCK
  END BLOCK ! { dg-error "Expected block name of 'myname2'" }
END PROGRAM main ! { dg-error "Expecting END BLOCK" }
! { dg-excess-errors "Unexpected end of file" }
