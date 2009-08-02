! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR 40848: [4.5 Regression] ICE with alternate returns
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>

MODULE TT

INTERFACE M
 MODULE PROCEDURE M1,M2
END INTERFACE

CONTAINS

 SUBROUTINE M1(I,*)
   INTEGER :: I
   RETURN 1
 END SUBROUTINE

 SUBROUTINE M2(I,J)
   INTEGER :: I,J
 END SUBROUTINE

END MODULE


  USE TT
  CALL M(1,*2)
  CALL ABORT()
2 CONTINUE
END

! { dg-final { cleanup-modules "tt" } }

