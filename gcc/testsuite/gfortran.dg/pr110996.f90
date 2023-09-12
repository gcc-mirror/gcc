! { dg-do compile }
!
! PR fortran/110996
! This example used to result in memory errors and sometimes internal compiler
! errors, because the rejection of the subroutine statement was causing the
! symbol D to be freed without also freeing the symbol C which remained in the
! namespace with a dangling pointer to D.
!
! Original testcase from Jeremy Bennett  <jeremy.bennett@embecosm.com>

PROGRAM p
CONTAINS
  SUBROUTINE c(d) e { dg-error "Syntax error" }
  SUBROUTINE f
  END
END
