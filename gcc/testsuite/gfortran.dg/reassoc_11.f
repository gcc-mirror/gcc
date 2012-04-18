! { dg-do compile }
! { dg-options "-O3 -ffast-math" }

! This tests only for compile-time failure, which formerly occurred
! when a __builtin_powi was introduced by reassociation in a bad place.

      SUBROUTINE GRDURBAN(URBWSTR, ZIURB, GRIDHT)

      IMPLICIT NONE
      INTEGER :: I
      REAL :: SW2, URBWSTR, ZIURB, GRIDHT(87)

      SAVE 

      SW2 = 1.6*(GRIDHT(I)/ZIURB)**0.667*URBWSTR**2

      END
