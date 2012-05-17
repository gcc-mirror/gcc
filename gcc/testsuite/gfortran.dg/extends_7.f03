! { dg-do compile }
! Check for re-definition of inherited components in the sub-type.

MODULE m1
  IMPLICIT NONE

  TYPE supert
    INTEGER :: c1
    INTEGER, PRIVATE :: c2
  END TYPE supert

END MODULE m1

MODULE m2
  USE m1 ! { dg-error "already in the parent type" }
  IMPLICIT NONE

  TYPE, EXTENDS(supert) :: subt
    INTEGER :: c1 ! { dg-error "already in the parent type" }
    INTEGER :: c2 ! { dg-error "already in the parent type" }
  END TYPE subt

END MODULE m2
