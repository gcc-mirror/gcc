! { dg-do compile }
!
! PR fortran/48588
!
! Contributed by Andres Legarra.
!

MODULE LA_PRECISION
IMPLICIT NONE
INTEGER, PARAMETER :: dp = KIND(1.0D0)
END MODULE LA_PRECISION

module lapack90
INTERFACE
  SUBROUTINE DGESV_F90( A, B, IPIV, INFO )
    USE la_precision, ONLY: wp => dp
    IMPLICIT NONE
    INTEGER, INTENT(OUT), OPTIONAL         :: INFO
    INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IPIV(:)
    REAL(WP), INTENT(IN OUT)               :: A(:,:), B(:,:)
  END SUBROUTINE DGESV_F90
END INTERFACE
end module

SUBROUTINE DGESV_F90( A, B, IPIV, INFO )
  USE la_precision, ONLY: wp => dp
  IMPLICIT NONE
  INTEGER, INTENT(OUT), OPTIONAL         :: INFO
  INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IPIV(:)
  REAL(WP), INTENT(IN OUT)               :: A(:,:), B(:,:)
END SUBROUTINE DGESV_F90

MODULE DENSEOP
  USE LAPACK90
  implicit none
  integer, parameter :: r8 = SELECTED_REAL_KIND( 15, 307 )
  real(r8)::denseop_tol=1.d-50

  CONTAINS

  SUBROUTINE GEINV8 (x)
   real(r8)::x(:,:)
   real(r8),allocatable::x_o(:,:)
   allocate(x_o(size(x,1),size(x,1)))
   CALL dgesv_f90(x,x_o)
   x=x_o
  END SUBROUTINE GEINV8
END MODULE DENSEOP
