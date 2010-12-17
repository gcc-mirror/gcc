! { dg-do compile }
!
! PR 45933: [4.6 regression] [OOP] ICE in gfc_add_component_ref, at fortran/class.c:77
!
! Contributed by Mark Rashid <mmrashid@ucdavis.edu>

MODULE DEF1
  TYPE :: DAT
    INTEGER :: NN
  CONTAINS
    PROCEDURE :: LESS_THAN
    GENERIC :: OPERATOR (.LT.) => LESS_THAN
  END TYPE
CONTAINS
  LOGICAL FUNCTION LESS_THAN(A, B)
    CLASS (DAT), INTENT (IN) :: A, B
    LESS_THAN = (A%NN .LT. B%NN)
  END FUNCTION
END MODULE

PROGRAM P
  USE DEF1
  TYPE NODE
    TYPE (DAT), POINTER :: PT
  END TYPE
  CLASS (NODE),POINTER :: A, B
  PRINT *, A%PT .LT. B%PT
END

! { dg-final { cleanup-modules "DEF1" } }
