! { dg-options "-O3 -ffast-math" }

      DIMENSION FPQR(25,25,25)
      INTEGER P,Q,R
            DO 130 R=1,N4MAX,2
               IF(P.GT.1) THEN
                  FPQR(P,Q,R)= RM2*FPQR(P,Q,R-2)*REC(P+Q+R-2)
               END IF
  130       RM2= RM2+TWO
      END
