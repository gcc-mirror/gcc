! { dg-do compile }

!$omp parallel
!$omp critical
       goto 10		! { dg-error "invalid (exit|branch)" }
!$omp end critical
 10    x = 1
!$omp end parallel

       end
