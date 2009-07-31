      SUBROUTINE BFN(X,BF)
      DIMENSION BF(13)
      DIMENSION FACT(17)
      DO 70 M=0,LAST
         XF = 1
         IF(M.NE.0) XF = FACT(M)
         Y = Y + XF
 70   CONTINUE
      BF(1)=Y
      END
