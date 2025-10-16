! { dg-do compile }

! This test case checks that a non-executable OpenMP directive is accepted 
! as intervening code.

SUBROUTINE test1(x_min, x_max, y_min, y_max, xarea, vol_flux_x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: x_min, x_max, y_min, y_max

  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: xarea
  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: vol_flux_x

  INTEGER :: j,k

  !$omp do collapse(2)
  DO k=y_min,y_max
  !$omp nothing
    DO j=x_min,x_max
      vol_flux_x(j,k)=0.25_8*xarea(j,k)
    ENDDO
  ENDDO

END SUBROUTINE test1

SUBROUTINE test2(x_min, x_max, y_min, y_max, x, z, vol_flux_x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: x_min, x_max, y_min, y_max

  REAL(KIND=8) :: x, z
  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: vol_flux_x

  INTEGER :: j,k

  !$omp do collapse(2)
  DO k=y_min,y_max
  !$omp assume holds(x>1)
    z = abs(x-1)
  !$omp end assume
    DO j=x_min,x_max
      vol_flux_x(j,k)=0.25_8*z
    ENDDO
  ENDDO

END SUBROUTINE test2

SUBROUTINE test3(x_min, x_max, y_min, y_max, z, vol_flux_x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: x_min, x_max, y_min, y_max

  REAL(KIND=8) :: z
  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: vol_flux_x

  INTEGER :: j,k

  !$omp do collapse(2)
  DO k=y_min,y_max
  !$omp error at(compilation)   ! { dg-error "OMP ERROR encountered at" }
    DO j=x_min,x_max
      vol_flux_x(j,k)=0.25_8*z
    ENDDO
  ENDDO

END SUBROUTINE test3

SUBROUTINE test4(x_min, x_max, y_min, y_max, z, vol_flux_x)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: x_min, x_max, y_min, y_max

  REAL(KIND=8) :: z
  REAL(KIND=8), DIMENSION(x_min:x_max,y_min:y_max) :: vol_flux_x

  INTEGER :: j,k

  !$omp do collapse(2)
  DO k=y_min,y_max
  !$omp error at(execution)   ! { dg-error "OMP DO cannot contain OpenMP directive in intervening code" }
    DO j=x_min,x_max
      vol_flux_x(j,k)=0.25_8*z
    ENDDO
  ENDDO

END SUBROUTINE test4
