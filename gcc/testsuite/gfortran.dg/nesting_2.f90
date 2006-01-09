! check to make the nested function dawsonseries_v gets the correct
! fake return decl and that the outer (dawson_v) has an assignment of
! just the fake return decl for real and not the inner's return decl.
! { dg-do compile }
FUNCTION dawson_v()
  IMPLICIT NONE
  REAL  :: dawson_v
  dawson_v = 1.0

  CONTAINS
    FUNCTION dawsonseries_v()
      IMPLICIT NONE
      REAL, DIMENSION(1) :: dawsonseries_v
      dawsonseries_v=1.0
    END FUNCTION dawsonseries_v
END FUNCTION dawson_v
