! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-options "-O2 -w" }
C fconc64.F, from CERNLIB (simplified)

      FUNCTION DFCONC(X,TAU,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMPLEX*16 WGAMMA,WLOGAM
      COMPLEX*16 CGM,CLG,CRG,I,A,B,C,TI,R,RR,U(0:3),V(0:3),W(19)
      LOGICAL LM0,LM1,LTA
      CHARACTER NAME*(*)
      CHARACTER*80 ERRTXT
      PARAMETER (NAME = 'RFCONC/DFCONC')
      DIMENSION T(7),H(9),S(5),P(11),D(-1:6)
      PARAMETER (PI  = 3.14159 26535 89793 24D+0)
      PARAMETER (RPI = 1.77245 38509 05516 03D+0)
      PARAMETER (I = (0,1))
      PARAMETER (Z1 = 1, HF = Z1/2, TH = 1+HF, C1 = Z1/10, C2 = Z1/5)
      PARAMETER (RPH = 2/PI, RPW = 2/RPI, TW = 20, NMAX = 200)
      DATA EPS /1D-14/
      ASSIGN 1 TO JP
      GO TO 20
    1 ASSIGN 2 TO JP
      GO TO 20
    2 IF(LM1) FC=2*FC/SQRT(1-X1)
      GO TO 99
   12 ASSIGN 3 TO JP
      GO TO 20
    3 IF(LM1) FC=SIGN(HF,1-X)*(TAU**2+HF**2)*SQRT(ABS(X**2-1))*FC
      GO TO 99
   13 ASSIGN 4 TO JP
      GO TO 20
    4 R1=EXP((TI-HF)*LOG(X+X)+CLG(1+TI)-CLG((TH-FM)+TI))*
     1        R*((HF-FM)+TI)/TI
      FC=RPW*R1
      IF(LM1) FC=FC/SQRT(1-X1)
      GO TO 99
   20 IF(LTA) THEN
       IF(ABS(R-RR) .LT. EPS) GO TO JP, (1,2,3,4)
      ELSE
       W(1)=X1*A*B/C
       R=1+W(1)
       DO 23 N = 1,NMAX
       RR=R
       W(1)=W(1)*X1*(A+FN)*(B+FN)/((C+FN)*(FN+1))
       IF(ABS(R-RR) .LT. EPS) GO TO JP, (1,2,3,4)
   23  CONTINUE
      END IF
   99 DFCONC=FC
      RETURN
  101 FORMAT('ILLEGAL ARGUMENT(S)  X = ',D15.8,' TAU = ',D15.8,
     1       ' M = ',I3)
  102 FORMAT('CONVERGENCE PROBLEM FOR HYPERGEOMETRIC FUNCTION, X = ',
     1        D15.8)
      END
