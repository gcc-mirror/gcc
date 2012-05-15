! { dg-do compile }
! Tests the fix for PR30883 in which interface functions and
! their results did not get an implicit type.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M1
  IMPLICIT NONE
CONTAINS
  SUBROUTINE S1(F1, F2, G1, G2)
    INTERFACE
      FUNCTION F1(i, a)
      END FUNCTION F1
      FUNCTION F2(i, a)
        implicit complex (a-z)
      END FUNCTION F2
    END INTERFACE
    INTERFACE
      FUNCTION g1(i, a) result(z)
      END FUNCTION g1
      FUNCTION g2(i, a) result(z)
        implicit complex (a-z)
      END FUNCTION g2
    END INTERFACE
  END SUBROUTINE S1
END MODULE

END
