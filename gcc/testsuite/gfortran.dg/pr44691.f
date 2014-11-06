C PR rtl-optimization/44691
C { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } }
C { dg-options "-O2 -fselective-scheduling2" }

      SUBROUTINE ORIEN(IW,NATOT,NTOTORB,NATORB,P,T)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION NATORB(NATOT),P(NTOTORB*(NTOTORB+1)/2)
      DIMENSION T(NTOTORB,NTOTORB)
      DO 9000 IATOM=1,NATOT
         ILAST = NTOTORB
         IF (IATOM.NE.NATOT) ILAST=NATORB(IATOM+1)-1
         DO 8000 IAOI=NATORB(IATOM),ILAST
            DO 7000 IAOJ = IAOI+1,ILAST
               R2 = 0.0D+00
               R3 = 0.0D+00
               DO 6000 INOTA=1,NATOT
                  DO 5000 IK=NATORB(INOTA),NTOTORB
                     IMAI=MAX(IK,IAOI)
                     IMII=MIN(IK,IAOI)
                     IMAJ=MAX(IK,IAOJ)
                     IMIJ=MIN(IK,IAOJ)
                     IKI=(IMAI*(IMAI-1))/2 + IMII
                     IKJ=(IMAJ*(IMAJ-1))/2 + IMIJ
                     PIKI=P(IKI)
                     PIKJ=P(IKJ)
                     R2 = R2 + (PIKI**4)-6*(PIKI*PIKI*PIKJ*PIKJ)+(PIKJ)
 5000             CONTINUE
 6000          CONTINUE
               R2 = (R2/4.0D+00)
               Q = SQRT(R2*R2 + R3*R3)
               IF (Q.LT.1.0D-08) GO TO 7000
               A = COS(THETA)
               B = -SIN(THETA)
               CALL ROT1INT(NTOTORB,IAOI,IAOJ,A,B,P)
 7000       CONTINUE
 8000    CONTINUE
 9000 CONTINUE
      RETURN
      END


