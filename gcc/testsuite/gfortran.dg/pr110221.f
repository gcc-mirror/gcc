C PR middle-end/68146
C { dg-do compile }
C { dg-options "-O2 -w" }
C { dg-additional-options "-mavx512f --param vect-partial-vector-usage=2" { target avx512f } }
      SUBROUTINE CJYVB(V,Z,V0,CBJ,CDJ,CBY,CYY)
      IMPLICIT DOUBLE PRECISION (A,B,G,O-Y)
      IMPLICIT COMPLEX*16 (C,Z)
      DIMENSION CBJ(0:*),CDJ(0:*),CBY(0:*)
      N=INT(V)
      CALL GAMMA2(VG,GA)
      DO 65 K=1,N
        CBY(K)=CYY
65    CONTINUE
      CDJ(0)=V0/Z*CBJ(0)-CBJ(1)
      DO 70 K=1,N
70      CDJ(K)=-(K+V0)/Z*CBJ(K)+CBJ(K-1)
      END
