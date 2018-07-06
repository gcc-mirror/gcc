! { dg-do run }
! Checks the fix for PR31205, in which temporaries were not
! written for the interface assignment and the parentheses below.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE TT
 TYPE data_type
   INTEGER :: I=2
 END TYPE data_type
 INTERFACE ASSIGNMENT (=)
   MODULE PROCEDURE set
 END INTERFACE
CONTAINS
  PURE SUBROUTINE set(x1,x2)
    TYPE(data_type), INTENT(IN) :: x2
    TYPE(data_type), INTENT(OUT) :: x1
    CALL S1(x1,x2)
  END SUBROUTINE
  PURE SUBROUTINE S1(x1,x2)
    TYPE(data_type), INTENT(IN) :: x2
    TYPE(data_type), INTENT(OUT) :: x1
    x1%i=x2%i
  END SUBROUTINE
END MODULE

USE TT
TYPE(data_type) :: D,E

D%I=4
D=D

E%I=4
CALL set(E,(E))

IF (D%I.NE.4) STOP 1
IF (4.NE.E%I) STOP 2
END
