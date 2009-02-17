C PR tree-optimization/36922
C { dg-do compile }
C { dg-options "-O2 -ftree-loop-linear" }
      SUBROUTINE PR36922(N,F,Z,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(23821),Z(0:2*N+1),F(0:2*N)
      I=0
      DO L=0,N
        DO M=0,L
          DO M2=M,L
            I=I+1
            C(I)=F(L+M)*F(L-M)*Z(L-M2)/(F(M2+M)*F(M2-M)*F(L-M2)*F(L-M2))
          ENDDO
        ENDDO
      ENDDO
      END
