      DIMENSION FF(19)
      COMMON UF(9)
         CALL RYSNOD(K)
      DO 150 K=2,N
         JMAX=K-1
            DUM = ONE/FF(1)
            DO 110 J=1,JMAX
               DUM=DUM+POLY*POLY
  110       CONTINUE
  150 CONTINUE
         UF(K)=DUM/(ONE-DUM)
      END
