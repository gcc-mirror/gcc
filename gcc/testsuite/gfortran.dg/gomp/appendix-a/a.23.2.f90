! { dg-do compile }
      SUBROUTINE A23_2_GOOD()
         COMMON /C/ X,Y
         REAL X, Y
         INTEGER I
!$OMP PARALLEL
!$OMP DO PRIVATE(/C/)
           DO I=1,1000
             ! do work here
           ENDDO
!$OMP END DO
!
!$OMP DO PRIVATE(X)
           DO I=1,1000
             ! do work here
           ENDDO
!$OMP END DO
!$OMP END PARALLEL
       END SUBROUTINE A23_2_GOOD
