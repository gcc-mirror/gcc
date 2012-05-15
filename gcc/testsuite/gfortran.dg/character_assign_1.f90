! { dg-do compile }
! Tests the fix for PR35702, which caused an ICE because the types in the assignment
! were not translated to be the same.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
MODULE TESTS
  TYPE UNSEQ
    CHARACTER(1) :: C
  END TYPE UNSEQ       
CONTAINS
  SUBROUTINE CG0028 (TDA1L, TDA1R, nf0, nf1, nf2, nf3)
    TYPE(UNSEQ) TDA1L(NF3)
    TDA1L(NF1:NF2:NF1)%C = TDA1L(NF0+2:NF3:NF2/2)%C
  END SUBROUTINE
END MODULE TESTS
