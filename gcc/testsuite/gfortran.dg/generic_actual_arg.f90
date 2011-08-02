! { dg-do compile }
! Tests fix for PR20886 in which the passing of a generic procedure as
! an actual argument was not detected.
!
! The second module and the check that CALCULATION2 is a good actual
! argument was added following the fix for PR26374.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk> 
!
MODULE TEST
INTERFACE CALCULATION
  MODULE PROCEDURE C1, C2
END INTERFACE
CONTAINS
SUBROUTINE C1(r)
 INTEGER :: r
END SUBROUTINE
SUBROUTINE C2(r)
 REAL :: r
END SUBROUTINE
END MODULE TEST

MODULE TEST2
INTERFACE CALCULATION2
  MODULE PROCEDURE CALCULATION2, C3
END INTERFACE
CONTAINS
SUBROUTINE CALCULATION2(r)
 INTEGER :: r
END SUBROUTINE
SUBROUTINE C3(r)
 REAL :: r
END SUBROUTINE
END MODULE TEST2
    
USE TEST
USE TEST2
CALL F(CALCULATION)  ! { dg-error "GENERIC procedure" } 

CALL F(CALCULATION2) ! OK because there is a same name specific, but: ! { dg-error "More actual than formal arguments" }
END

SUBROUTINE F()
END SUBROUTINE
! { dg-final { cleanup-modules "test test2" } }
