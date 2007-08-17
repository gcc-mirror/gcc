! { dg-do compile }
! Tests the fix PR29744, which is really a repeat of PR19362.
! The problem came about because the test for PR19362 shifted
! the fix to a subroutine, rather than the main program that
! it originally occurred in.  Fixes for subsequent PRs introduced
! a difference between the main program and a contained procedure
! that resulted in the compiler going into an infinite loop.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
! and originally by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org> 
!
MODULE M
  TYPE T0
    SEQUENCE
    INTEGER I
  END TYPE
END

PROGRAM MAIN
  USE M, T1 => T0
  TYPE T0
    SEQUENCE
    INTEGER I
  END TYPE
  TYPE(T0) :: BAR
  TYPE(T1) :: BAZ
  BAZ = BAR
END
! { dg-final { cleanup-modules "m" } }

