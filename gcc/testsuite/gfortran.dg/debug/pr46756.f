C PR debug/46756, reduced from ../20010519-1.f
C { dg-do compile }
C { dg-options "-O -fcompare-debug" }

      LOGICAL QDISK,QDW,QCMPCT
      LOGICAL LNOMA,LRAISE,LSCI,LBIG
      ASSIGN 801 TO I800 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 800
 801  CONTINUE
      ASSIGN 761 TO I760 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
 761  CONTINUE
      IF(LSCI) THEN
         DO I=1,LENCM
         ENDDO
      ENDIF
      DO WHILE((CVGMX.GT.TOLDIM).AND.(ITER.LT.ITMX))
         IF(.NOT.QDW) THEN
            ASSIGN 641 to I640 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
            GOTO 640
 641        CONTINUE
         ENDIF
      ENDDO
      GOTO 700
 640  CONTINUE
      GOTO I640 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
 700  CONTINUE
      GOTO I760 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
 800  CONTINUE
      GOTO I800 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
      END
