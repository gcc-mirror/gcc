! { dg-do compile }

! This test case checks that the inner metadirective is accepted as intervening
! code since it resolves to 'omp nothing'.

SUBROUTINE test1(x_min, x_max, y_min, y_max, xarea, vol_flux_x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: x_min, x_max, y_min, y_max

  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: xarea
  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: vol_flux_x

  INTEGER :: j,k

  !$omp metadirective                                                          &
  !$omp  when(user={condition(.false.)}:                              &
  !$omp      target teams distribute parallel do simd collapse(2))             &
  !$omp  when(user={condition(.false.)}:                          &
  !$omp      target teams distribute parallel do)                              &
  !$omp  default(                                                              &
  !$omp      target teams loop collapse(2))
  DO k=y_min,y_max
    !$omp metadirective when(user={condition(.false.)}: simd)
    DO j=x_min,x_max
      vol_flux_x(j,k)=0.25_8*xarea(j,k)
    ENDDO
  ENDDO

END SUBROUTINE test1
