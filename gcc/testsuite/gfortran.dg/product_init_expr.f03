! { dg-do run }
! { dg-options "-fno-inline" }
!
! PRODUCT as initialization expression.
!
! This test compares results of simplifier of PRODUCT
! with the corresponding inlined or library routine(s).
!

  IMPLICIT NONE

  INTEGER, PARAMETER :: imatrix(2,4) = RESHAPE ([ 1, 2, 3, 4, 5, 6, 7, 8 ], [2, 4] )
  INTEGER, PARAMETER :: imatrix_prod = PRODUCT (imatrix)
  INTEGER, PARAMETER :: imatrix_prod_d1(4) = PRODUCT (imatrix, dim=1)
  INTEGER, PARAMETER :: imatrix_prod_d2(2) = PRODUCT (imatrix, dim=2)
  LOGICAL, PARAMETER :: i_equal_prod = ALL ([PRODUCT( imatrix_prod_d1 ) ==  PRODUCT ( imatrix_prod_d2 ), &
                                             PRODUCT( imatrix_prod_d1 ) == imatrix_prod])
  LOGICAL, PARAMETER :: i_empty_prod = PRODUCT(imatrix, mask=.FALSE.) == 1

  REAL, PARAMETER :: rmatrix(2,4) = RESHAPE ([ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ], [2, 4] )
  REAL, PARAMETER :: rmatrix_prod = PRODUCT (rmatrix)
  REAL, PARAMETER :: rmatrix_prod_d1(4) = PRODUCT (rmatrix, dim=1)
  REAL, PARAMETER :: rmatrix_prod_d2(2) = PRODUCT (rmatrix, dim=2)
  LOGICAL, PARAMETER :: r_equal_prod = ALL ([PRODUCT( rmatrix_prod_d1 ) ==  PRODUCT ( rmatrix_prod_d2 ), &
                                             PRODUCT( rmatrix_prod_d1 ) == rmatrix_prod])
  LOGICAL, PARAMETER :: r_empty_prod = PRODUCT(rmatrix, mask=.FALSE.) == 1.0

  IF (.NOT. ALL ([i_equal_prod, i_empty_prod])) CALL abort()
  IF (.NOT. ALL ([r_equal_prod, r_empty_prod])) CALL abort()

  CALL ilib (imatrix, imatrix_prod)
  CALL ilib_with_dim (imatrix, 1, imatrix_prod_d1)
  CALL ilib_with_dim (imatrix, 2, imatrix_prod_d2)
  CALL rlib (rmatrix, rmatrix_prod)
  CALL rlib_with_dim (rmatrix, 1, rmatrix_prod_d1)
  CALL rlib_with_dim (rmatrix, 2, rmatrix_prod_d2)

CONTAINS
  SUBROUTINE ilib (array, result)
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(in) :: result
    IF (PRODUCT(array) /= result) CALL abort()
  END SUBROUTINE

  SUBROUTINE ilib_with_dim (array, dim, result)
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(iN)                 :: dim
    INTEGER, DIMENSION(:), INTENT(in)   :: result
    IF (ANY (PRODUCT (array, dim=dim) /= result)) CALL abort()
  END SUBROUTINE

  SUBROUTINE rlib (array, result)
    REAL, DIMENSION(:,:), INTENT(in) :: array
    REAL, INTENT(in) :: result
    IF (ABS(PRODUCT(array) - result) > 2e-6) CALL abort()
  END SUBROUTINE

  SUBROUTINE rlib_with_dim (array, dim, result)
    REAL, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(iN)              :: dim
    REAL, DIMENSION(:), INTENT(in)   :: result
    IF (ANY (ABS(PRODUCT (array, dim=dim) - result) > 2e-6)) CALL abort()
  END SUBROUTINE
END


