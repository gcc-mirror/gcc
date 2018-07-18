! { dg-do compile }

      SUBROUTINE A23_5_WRONG()
        COMMON /C/ X,Y
! Incorrect: common block C cannot be declared both
! shared and private
!$OMP PARALLEL PRIVATE (/C/), SHARED(/C/)
          ! { dg-error "Symbol 'y' present" "" { target *-*-* } .-1 }
          ! { dg-error "Symbol 'x' present" "" { target *-*-* } .-2 }
          ! do work here
!$OMP END PARALLEL
      END SUBROUTINE A23_5_WRONG
