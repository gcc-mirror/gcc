! { dg-do compile }
! Tests fix for PR20886 in which the passing of a generic procedure as
! an actual argument was not detected.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk> 
!
MODULE TEST
INTERFACE CALCULATION
  MODULE PROCEDURE C1,C2
END INTERFACE
CONTAINS
SUBROUTINE C1(r)
 INTEGER :: r
END SUBROUTINE
SUBROUTINE C2(r)
 REAL :: r
END SUBROUTINE
END MODULE TEST
    
USE TEST
CALL F(CALCULATION) ! { dg-error "GENERIC non-INTRINSIC procedure" } 
END

SUBROUTINE F()
END SUBROUTINE