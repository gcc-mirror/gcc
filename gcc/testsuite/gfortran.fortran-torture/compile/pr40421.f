      SUBROUTINE VROT2(N,DIS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ZERO=0.0D+00)
      COMMON /SYMSPD/ PTR(3,144)
      DIMENSION DIS(3,2),TMP(3,2)
      DO I = 1,3
        TMP1 = ZERO
        DO J = 1,3
          TMP1 = TMP1 + PTR(I,N+J)
        END DO
        TMP(I,1) = TMP1
      END DO
      DO I = 1,3
        DIS(I,1) = TMP(I,1)
      END DO
      RETURN
      END

