      SUBROUTINE CAMB(RX2,RTX,NUM)
      DIMENSION RX2(NUM,NUM),RTX(NUM,NUM)
      DO I=1,NUM
        DO J=1,I
          DO M=1,NUM
            RX2(I,J)=RX2(I,J)+RTX(M,I)
          END DO
        END DO
      END DO
      IF (RX2(I,1).LE.EIGCT2) THEN
      RTX(I,1)=4.0D+00
      END IF
      END
