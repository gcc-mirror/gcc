! PR middle-end/57089
! { dg-do compile }
! { dg-options "-O -fopenmp" }
  SUBROUTINE T()
    INTEGER            :: npoints, grad_deriv
    SELECT CASE(grad_deriv)
    CASE (0)
       !$omp do
       DO ii=1,npoints
       END DO
    END SELECT
  END SUBROUTINE 
