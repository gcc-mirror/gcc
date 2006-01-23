! { dg-do compile }
! Tests the fix for the regression caused by the patch for PR20869
! which itself is tested and described by intrinsic_external_1.f90
!
! reported to the fortran list by Dominique Dhumieres  dominiq@lps.ens.fr

MODULE global
   INTERFACE
      SUBROUTINE foo(i, j)
      IMPLICIT NONE
      INTEGER :: j
      integer, DIMENSION(j,*) :: i ! This constituted usage of j and so triggered....
      INTENT (IN) j  ! Would give "Cannot change attributes of symbol at (1) after it has been used"
      INTENT (INOUT) i
      END SUBROUTINE foo
   END INTERFACE
END MODULE global
