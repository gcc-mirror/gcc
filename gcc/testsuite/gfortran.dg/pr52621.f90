! { dg-do compile }
! { dg-options "-O2 -fprefetch-loop-arrays -w" }

      SUBROUTINE GHDSYM(IZ,IS,LMMAX,S,LMS,Y,L2M,DRL,NLAY2,K0,DCUT)!,
!
      COMPLEX Y(L2M,L2M),H(33),S(LMS)
      COMPLEX RU,CI,CZ,K0,FF,Z,Z1,Z2,Z3,ST
!
      DO 140 KK=1,4
            DO 130 L=1,L2M
               L1=L*L-L
               DO 120 M=1,L
                  IPM=L1+M
                  IMM=L1-M+2
                  S(IPM)=S(IPM)+Z3*Y(L,M)
                  IF (M.NE.1) S(IMM)=S(IMM)+Z3*Y(M-1,L)*CSGN
120            CONTINUE
130         CONTINUE
140   CONTINUE
      END
