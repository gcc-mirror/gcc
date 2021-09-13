! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Check fix for PR98517
!
! Contributed by Eric Reischer  <emr-gnu@hev.psu.edu>
!
      SUBROUTINE TEST_BUG
      IMPLICIT NONE

      CHARACTER*(*) DEF_VAL
      PARAMETER (DEF_VAL = 'ABCDEFGH')

      STRUCTURE /SOME_STRUCT/
          CHARACTER*64    SOME_VAR /DEF_VAL/
      END STRUCTURE

      END
