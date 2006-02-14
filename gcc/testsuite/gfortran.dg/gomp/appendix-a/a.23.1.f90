! { dg-do compile }
      SUBROUTINE A23_1_GOOD()
        COMMON /C/ X,Y
        REAL X, Y
!$OMP PARALLEL PRIVATE (/C/)
          ! do work here
!$OMP END PARALLEL
!$OMP PARALLEL SHARED (X,Y)
          ! do work here
!$OMP END PARALLEL
      END SUBROUTINE A23_1_GOOD
