! { dg-do compile }
! { dg-options "-c" }
! Testcase from PR fortran/25094
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>

MODULE M1
  TYPE T1
    INTEGER :: I
  END TYPE T1
  INTERFACE I
    MODULE PROCEDURE F1        ! { dg-error "PUBLIC interface" }
  END INTERFACE
  PRIVATE ! :: T1,F1
  PUBLIC  :: I
CONTAINS
  INTEGER FUNCTION F1(D)
    TYPE(T1) :: D
    F1 = D%I
  END FUNCTION
END MODULE

! { dg-final { cleanup-modules "M1" } }
