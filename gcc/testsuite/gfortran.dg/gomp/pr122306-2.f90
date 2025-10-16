! { dg-do compile }

! This test case checks that various user-condition context selectors correctly
! parsed and resolved.

SUBROUTINE test1(x_min, x_max, vol_flux_x)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: x_min, x_max
  REAL(KIND=8), DIMENSION(x_min:x_max) :: vol_flux_x
  integer, parameter :: one = 1
  INTEGER :: j

   !$omp begin metadirective when(user={condition(one < 0)}: parallel)
    DO j=x_min,x_max
      vol_flux_x(j)=0.25_8
    ENDDO
   !$omp end metadirective
END SUBROUTINE test1

SUBROUTINE test2(x_min, x_max, vol_flux_x, flag)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: x_min, x_max
  REAL(KIND=8), DIMENSION(x_min:x_max) :: vol_flux_x
  LOGICAL :: flag
  INTEGER :: j

   !$omp begin metadirective when(user={condition(flag)}: parallel)
    DO j=x_min,x_max
      vol_flux_x(j)=0.25_8
    ENDDO
   !$omp end metadirective
END SUBROUTINE test2

