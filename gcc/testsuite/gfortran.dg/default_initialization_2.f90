! { dg-do compile }
! This tests the patch for PR29098, in which the presence of the default
! initializer would cause allocate to fail because the latter uses
! the interface assignment.  This, in its turn was failing because
! no expressions were found for the other components; and a FAILURE
! was returned from resolve_structure_cons.
!
! Contributed by Olav Vahtras  <vahtras@pdc.kth.se>
!
 MODULE MAT
   TYPE BAS
      INTEGER :: R = 0,C = 0
   END TYPE BAS
   TYPE BLOCK
      INTEGER, DIMENSION(:), POINTER ::  R,C
      TYPE(BAS), POINTER, DIMENSION(:) :: NO => NULL()
   END TYPE BLOCK
   INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE BLASSIGN
   END INTERFACE
   CONTAINS
      SUBROUTINE BLASSIGN(A,B)
      TYPE(BLOCK), INTENT(IN) :: B
      TYPE(BLOCK), INTENT(INOUT) :: A
      INTEGER I,N
      ! ...
      END SUBROUTINE BLASSIGN
 END MODULE MAT
PROGRAM TEST
USE MAT
TYPE(BLOCK) MATRIX
POINTER MATRIX
ALLOCATE(MATRIX)
END
