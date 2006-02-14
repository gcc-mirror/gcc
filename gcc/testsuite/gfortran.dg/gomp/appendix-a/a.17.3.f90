! { dg-do compile }
      SUBROUTINE A17_3_WRONG
        INTEGER:: I
        REAL:: R
        EQUIVALENCE(I,R)
!$OMP PARALLEL
!$OMP ATOMIC
            I=I+1
! incorrect because I and R reference the same location
! but have different types
!$OMP END PARALLEL
!$OMP PARALLEL
!$OMP ATOMIC
            R = R + 1.0
! incorrect because I and R reference the same location
! but have different types
!$OMP END PARALLEL
      END SUBROUTINE A17_3_WRONG
