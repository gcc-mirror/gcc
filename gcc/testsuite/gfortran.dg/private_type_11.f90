! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/38065
!
! Reported by Norman S. Clerman
! and reduced by Joost VandeVondele
!
MODULE M1
  IMPLICIT NONE
  PRIVATE
  TYPE T1
   INTEGER :: I1
  END TYPE T1
  PUBLIC :: S1,F2
CONTAINS
  SUBROUTINE S1
  CONTAINS
   TYPE(T1) FUNCTION F1()
   END FUNCTION F1
  END SUBROUTINE S1
  TYPE(T1) FUNCTION F2()
  END FUNCTION F2
END MODULE M1
! { dg-final { cleanup-modules "m1" } }
