! { dg-do compile }

        SUBROUTINE A23_4_WRONG()
        COMMON /C/ X,Y
! Incorrect because X is a constituent element of C
!$OMP PARALLEL PRIVATE(/C/), SHARED(X)	! { dg-error "Symbol 'x' present" }
          ! do work here
!$OMP END PARALLEL
      END SUBROUTINE A23_4_WRONG
