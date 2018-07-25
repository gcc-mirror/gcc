! { dg-do compile }
! { dg-options "-Ofast -fwrapv -std=legacy" }
! { dg-additional-options "-march=broadwell" { target x86_64-*-* i?86-*-* } }
      SUBROUTINE ECPDRA(IC4C,FP,FQ,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FP(*),FQ(*),G(*)
      DIMENSION CKLU(23,12,12)
!
      DO 240 I=IAMIN,IAMAX
         DO 240 J=JAMIN,MMAX
            DO 230 K=1,NPNP
               DO 230 L=1,K
                  DO 230 MU=1,2*L-1
                     CKLTEM= CKLU(MU,L,K)
                     IF(IC4C.LE.0) THEN
                        IF(ABS(CKLTEM).GT.TOL) SUM= SUM+FP(N)*CKLTEM
                     ELSE
                        IF(ABS(CKLTEM).GT.TOL) SUM= SUM+FQ(N)*CKLTEM
                     END IF
  230       N= N+1
            G(NN)= G(NN)+DUMJ*SUM
  240 NN= NN+1
      END
