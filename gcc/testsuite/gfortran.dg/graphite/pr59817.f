! { dg-do compile }
! { dg-options "-O2 -floop-nest-optimize" }
      SUBROUTINE PREPD(ICAST,ICAS,ICASX,ICAS1,ICAS2,NDET,NM,III,IMP,
     *                 CASMIN)
      LOGICAL CASMIN
      DIMENSION ICAST(NDET,NM),IMP(NM)
      IF(CASMIN) THEN
         DO K=1,NDET
            DO L=1,NM
               IF(L.EQ.K-1) ICAST(K,L) = 1
            END DO
         END DO
      END IF
      END SUBROUTINE
