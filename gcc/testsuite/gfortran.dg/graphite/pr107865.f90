! { dg-do compile }
! { dg-options "-O1 -floop-parallelize-all -ftree-parallelize-loops=2" }

      SUBROUTINE FNC (F,N)

      IMPLICIT REAL (A-H)
      DIMENSION F(N)

      DO I = 1, 6
         DO J = 1, 6
            IF (J .NE. I) THEN
               F(I) = F(I) + 1
            END IF
         END DO
      END DO

      RETURN
      END
