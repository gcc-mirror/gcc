! { dg-do compile }
!
! PR fortran/34660
!
! Check for elemental constrain C1277 (F2003).
! Contributed by Joost VandeVondele.
!
MODULE M1
IMPLICIT NONE
CONTAINS
 PURE ELEMENTAL SUBROUTINE S1(I,F)
   INTEGER, INTENT(IN) :: I
   INTERFACE
     PURE INTEGER FUNCTION F(I) ! { dg-error "Dummy procedure 'f' not allowed in elemental procedure" }
      INTEGER, INTENT(IN) :: I
     END FUNCTION F
   END INTERFACE
 END SUBROUTINE S1
END MODULE M1
