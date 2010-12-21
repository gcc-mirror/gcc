C { dg-do compile }
C { dg-options "-O" }
      IMPLICIT NONE
      INTEGER I640,I760,I800
      INTEGER I,ITER,ITMX,LENCM
      LOGICAL QDISK,QDW
      ASSIGN 801 TO I800
      GOTO I800
 801  CONTINUE
      ASSIGN 761 TO I760
 761  CONTINUE
      DO I=1,LENCM
      ENDDO
      DO WHILE(ITER.LT.ITMX)
         IF(QDW) THEN
            ASSIGN 641 to I640
            GOTO I760
 641        CONTINUE
         ENDIF
      ENDDO
      RETURN
      END

