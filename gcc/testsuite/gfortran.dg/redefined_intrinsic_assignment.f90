! { dg-do compile }
! Tests the fix for PR25077 in which no diagnostic was produced
! for the redefinition of an intrinsic type assignment.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE M1
 IMPLICIT NONE
 INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE T1 ! { dg-error "redefine an INTRINSIC type assignment" }
 END INTERFACE
CONTAINS
 SUBROUTINE T1(I,J)
   INTEGER, INTENT(OUT)  :: I
   INTEGER, INTENT(IN)  :: J
   I=-J
 END SUBROUTINE T1
END MODULE M1
