! { dg-do compile }
!
! NULL(...) pointer is not allowed as operand
! PR fortran/20888
!
! Contributed by Joost VandeVondele
!
PROGRAM main
  IMPLICIT NONE
  REAL, POINTER :: TEST
  NULLIFY(TEST)
  TEST => -NULL(TEST) ! { dg-error "Invalid context for NULL" }
  IF (TEST .EQ. NULL(TEST)) TEST=>NULL() ! { dg-error "Invalid context for NULL" }
  IF (NULL(TEST) .EQ. TEST) TEST=>NULL() ! { dg-error "Invalid context for NULL" }
END PROGRAM main
