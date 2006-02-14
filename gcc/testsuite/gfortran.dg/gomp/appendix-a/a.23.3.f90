! { dg-do compile }

      SUBROUTINE A23_3_GOOD()
        COMMON /C/ X,Y
!$OMP PARALLEL PRIVATE (/C/)
          ! do work here
!$OMP END PARALLEL
!$OMP PARALLEL SHARED (/C/)
          ! do work here
!$OMP END PARALLEL
      END SUBROUTINE A23_3_GOOD
