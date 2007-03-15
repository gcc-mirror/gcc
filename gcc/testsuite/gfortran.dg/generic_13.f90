! { dg-do compile }
! tests the patch for PR30870, in which the generic XX was rejected
! because the specific with the same name was not looked for.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE TEST
 INTERFACE xx
   MODULE PROCEDURE xx
 END INTERFACE
 public :: xx
CONTAINS
 SUBROUTINE xx(i)
  INTEGER :: I
  I=7
 END SUBROUTINE
END
MODULE TOO
CONTAINS
 SUBROUTINE SUB(xx,I)
  INTERFACE
    SUBROUTINE XX(I)
        INTEGER :: I
    END SUBROUTINE
  END INTERFACE
  CALL XX(I)
 END SUBROUTINE
END MODULE TOO
PROGRAM TT
 USE TEST
 USE TOO
 INTEGER :: I
 CALL SUB(xx,I)
 IF (I.NE.7) CALL ABORT()
END PROGRAM
! { dg-final { cleanup-modules "test too" } }
