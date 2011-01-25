      SUBROUTINE TFTRAB(A,NA)
      DIMENSION A(NA,NA)
      DO 160 K=1,NA
         DUM = DUM + A(K,I)
 160  CONTINUE
      DO 180 I=1,NA
         A(I,J) = DUM
 180  CONTINUE
      END
