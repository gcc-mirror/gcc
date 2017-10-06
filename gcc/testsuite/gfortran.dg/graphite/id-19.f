      SUBROUTINE ECCODR(FPQR)
      DIMENSION FPQR(25,25,25)
      INTEGER P,Q,R
      DIMENSION REC(73)
      DO 150 P=1,N4MAX,2
         QM2=-ONE
         DO 140 Q=1,N4MAX,2
            DO 130 R=1,N4MAX,2
               IF(P.GT.1) THEN
                  FPQR(P,Q,R)= QM2*FPQR(P,Q-2,R)*REC(P+Q-2+R)
               END IF
  130       RM2= RM2+TWO
  140    QM2= QM2+TWO
  150 PM2= PM2+TWO
      END
