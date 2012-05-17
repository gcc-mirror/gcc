! { dg-do compile }
! Tests the fix for PR31494, where the call of sub2 would reference
! the variable, rather than the contained subroutine.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
MODULE ksbin2_aux_mod
REAL, DIMENSION(1) :: sub2
CONTAINS
  SUBROUTINE sub1
    CALL sub2
    CONTAINS 
      SUBROUTINE sub2
      END SUBROUTINE sub2
  END SUBROUTINE sub1
END MODULE ksbin2_aux_mod
