! { dg-do compile }

       SUBROUTINE A27()
         INTEGER I, A
!$OMP PARALLEL PRIVATE(A)
!$OMP PARALLEL DO PRIVATE(A)
           DO I = 1, 10
              ! do work here
          END DO
!$OMP END PARALLEL DO
!$OMP END PARALLEL
      END SUBROUTINE A27
