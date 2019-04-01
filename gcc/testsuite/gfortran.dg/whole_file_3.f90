! { dg-do compile }
! { dg-options "" }
! Tests the fix for PR26227 in which the interface mismatches
! below were not detected.
!
! Contributed by Andrew Pinski <pinskia@gcc.gnu.org>
!
      SUBROUTINE PHLOAD (READER,*)
      IMPLICIT NONE
      EXTERNAL         READER
      CALL READER (*1)
 1    RETURN 1
      END SUBROUTINE

      program test
      EXTERNAL R
      call PHLOAD (R, 1) ! { dg-error "Missing alternate return specifier" }
      CALL PHLOAD (R, 2) ! { dg-error "Missing alternate return specifier" }
      CALL PHLOAD (R, *999) ! This one is OK
 999  continue
      END program test
