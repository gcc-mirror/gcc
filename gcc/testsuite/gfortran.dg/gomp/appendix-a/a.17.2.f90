! { dg-do compile }

      SUBROUTINE SUB()
        COMMON /BLK/ R
        REAL R
!$OMP ATOMIC
        R = R + 1.0
      END SUBROUTINE SUB

      SUBROUTINE A17_2_WRONG()
      COMMON /BLK/ I
      INTEGER I
!$OMP PARALLEL
!$OMP ATOMIC
            I=I+1
          CALL SUB()
!$OMP END PARALLEL
      END SUBROUTINE A17_2_WRONG

