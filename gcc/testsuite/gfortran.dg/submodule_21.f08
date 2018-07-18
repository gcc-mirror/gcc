! { dg-do compile }
!
! Test the fix for PR78331.
!
! Reported on https://groups.google.com/forum/#!topic/comp.lang.fortran/NFCF9brKksg
!
MODULE MainModule
END MODULE MainModule

SUBMODULE (MainModule) MySub1
  IMPLICIT NONE
  INTEGER, PARAMETER :: a = 17
END SUBMODULE MySub1

PROGRAM MyProg
  USE MainModule
  WRITE(*,*) a
END PROGRAM MyProg
! { dg-excess-errors "does not contain a MODULE PROCEDURE" }
