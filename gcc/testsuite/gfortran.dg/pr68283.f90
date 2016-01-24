! { dg-do compile }
MODULE neb_utils
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=8
  TYPE neb_var_type
     REAL(KIND=dp), DIMENSION(:, :),  POINTER  :: xyz, int, wrk
  END TYPE neb_var_type
CONTAINS
  SUBROUTINE get_neb_force()
    INTEGER                                  :: i
    TYPE(neb_var_type), POINTER              :: forces
    REAL(KIND=dp), ALLOCATABLE, DIMENSION(:) :: dtmp1, wrk
    dtmp1 = forces%wrk(:,i)-dot_product_band ! { dg-error "Symbol 'dot_product_band' at .1. has no IMPLICIT type" }
  END SUBROUTINE get_neb_force
END MODULE neb_utils
