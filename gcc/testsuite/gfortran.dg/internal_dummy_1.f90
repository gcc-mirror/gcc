! { dg-do compile }
! Tests the fix for 20861, in which internal procedures were permitted to
! be dummy arguments.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
CALL DD(TT) ! { dg-error "is not allowed as an actual argument" }
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
