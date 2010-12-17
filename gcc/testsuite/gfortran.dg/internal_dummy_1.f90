! { dg-do compile }
! { dg-options "-std=f2003" }
! Tests the fix for 20861, in which internal procedures were permitted to
! be dummy arguments.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
CALL DD(TT) ! { dg-error "Fortran 2008: Internal procedure 'tt' is used as actual argument" }
CONTAINS
SUBROUTINE DD(F)
  INTERFACE
   SUBROUTINE F(X)
    REAL :: X
   END SUBROUTINE F
  END INTERFACE
END SUBROUTINE DD
SUBROUTINE TT(X)
  REAL :: X
END SUBROUTINE
END
