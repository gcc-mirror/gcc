! { dg-do compile }
! { dg-options "-O -finline-small-functions" }
! Tests the fix for PR45743 in which the compilation failed with an ICE
! internal compiler error: verify_stmts failed.  The source is the essential
! part of whole_file_3.f90.
!
! Contributed by Zdenek Sojka  <zsojka@seznam.cz>
!
      SUBROUTINE PHLOAD (READER,*)
      IMPLICIT NONE
      EXTERNAL         READER
      CALL READER (*1)
 1    RETURN 1
      END SUBROUTINE

      program test
      EXTERNAL R
      CALL PHLOAD (R, *999) ! This one is OK
 999  continue
      END program test
