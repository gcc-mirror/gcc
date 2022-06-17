! { dg-do compile }
! PR fortran/105501 - check for non-optional spaces between adjacent keywords

MODULE M
  TYPE T
     INTEGER I
  END TYPE
CONTAINS
  SUBROUTINE S(X)
    CLASS(T), POINTER :: X
    SELECTTYPE (X) ! blank between SELECT and TYPE is optional
    TYPEIS (T)     ! { dg-error "Mangled derived type definition" }
    END SELECT
  END SUBROUTINE
END MODULE
