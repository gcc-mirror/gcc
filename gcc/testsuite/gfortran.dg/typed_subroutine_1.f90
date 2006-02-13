! { dg-do compile }
! Tests the fix for 20858, in which the compiler failed to detect that
! a called object had a type.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
 INTEGER :: S ! { dg-error "has a type, which is not consistent with the CALL " }
 CALL S()     ! { dg-error "has a type, which is not consistent with the CALL " }
 END
 SUBROUTINE S
 END SUBROUTINE
