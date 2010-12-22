C { dg-do compile }
C { dg-options "-O" }
      IMPLICIT NONE
      INTEGER I640,I760,I800
      INTEGER I,ITER,ITMX,LENCM
      LOGICAL QDISK,QDW
      ASSIGN 801 TO I800        ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }

      GOTO I800                 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
 801  CONTINUE
      ASSIGN 761 TO I760        ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
 761  CONTINUE
      DO I=1,LENCM
      ENDDO
      DO WHILE(ITER.LT.ITMX)
         IF(QDW) THEN
            ASSIGN 641 to I640  ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
            GOTO I760           ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
 641        CONTINUE
         ENDIF
      ENDDO
      RETURN
      END

