! { dg-do run }
! { dg-options "-fno-inline" }
!
! SUM as initialization expression.
!
! This test compares results of simplifier of SUM 
! with the corresponding inlined or library routine(s).
!

  IMPLICIT NONE

  INTEGER, PARAMETER :: imatrix(2,4) = RESHAPE ([ 1, 2, 3, 4, 5, 6, 7, 8 ], [2, 4] )
  INTEGER, PARAMETER :: imatrix_sum = SUM (imatrix)
  INTEGER, PARAMETER :: imatrix_sum_d1(4) = SUM (imatrix, dim=1)
  INTEGER, PARAMETER :: imatrix_sum_d2(2) = SUM (imatrix, dim=2)
  LOGICAL, PARAMETER :: i_equal_sum = ALL ([SUM( imatrix_sum_d1 ) ==  SUM ( imatrix_sum_d2 ), &
                                            SUM( imatrix_sum_d1 ) == imatrix_sum])
  LOGICAL, PARAMETER :: i_empty_sum = SUM(imatrix, mask=.FALSE.) == 0

  REAL, PARAMETER :: rmatrix(2,4) = RESHAPE ([ 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8 ], [2, 4] )
  REAL, PARAMETER :: rmatrix_sum = SUM (rmatrix)
  REAL, PARAMETER :: rmatrix_sum_d1(4) = SUM (rmatrix, dim=1)
  REAL, PARAMETER :: rmatrix_sum_d2(2) = SUM (rmatrix, dim=2)
  LOGICAL, PARAMETER :: r_equal_sum = ALL ([SUM( rmatrix_sum_d1 ) ==  SUM ( rmatrix_sum_d2 ), &
                                            SUM( rmatrix_sum_d1 ) == rmatrix_sum])
  LOGICAL, PARAMETER :: r_empty_sum = SUM(rmatrix, mask=.FALSE.) == 0.0

  IF (.NOT. ALL ([i_equal_sum, i_empty_sum])) CALL abort()
  IF (.NOT. ALL ([r_equal_sum, r_empty_sum])) CALL abort()

  CALL ilib (imatrix, imatrix_sum)
  CALL ilib_with_dim (imatrix, 1, imatrix_sum_d1)
  CALL ilib_with_dim (imatrix, 2, imatrix_sum_d2)
  CALL rlib (rmatrix, rmatrix_sum)
  CALL rlib_with_dim (rmatrix, 1, rmatrix_sum_d1)
  CALL rlib_with_dim (rmatrix, 2, rmatrix_sum_d2)

CONTAINS
  SUBROUTINE ilib (array, result)
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(in) :: result
    IF (SUM(array) /= result) CALL abort()
  END SUBROUTINE

  SUBROUTINE ilib_with_dim (array, dim, result)
    INTEGER, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(iN)                 :: dim
    INTEGER, DIMENSION(:), INTENT(in)   :: result
    IF (ANY (SUM (array, dim=dim) /= result)) CALL abort()
  END SUBROUTINE

  SUBROUTINE rlib (array, result)
    REAL, DIMENSION(:,:), INTENT(in) :: array
    REAL, INTENT(in) :: result
    IF (ABS(SUM(array) - result) > 4e-6) CALL abort()
  END SUBROUTINE

  SUBROUTINE rlib_with_dim (array, dim, result)
    REAL, DIMENSION(:,:), INTENT(in) :: array
    INTEGER, INTENT(iN)              :: dim
    REAL, DIMENSION(:), INTENT(in)   :: result
    IF (ANY (ABS(SUM (array, dim=dim) - result) > 4e-6)) CALL abort()
  END SUBROUTINE
END


