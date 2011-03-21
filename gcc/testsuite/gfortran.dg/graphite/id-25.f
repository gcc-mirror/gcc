      SUBROUTINE TFTRAB(NA,NC,D,WRK)
      DIMENSION D(NA,NC), WRK(NA)
      DO 160 K=1,NA
         DUM = DUM + D(K,J)
 160  CONTINUE
      WRK(I) = DUM
      DO 180 I=1,NA
         D(I,J) = WRK(I)
 180  CONTINUE
      END
