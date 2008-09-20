C
      SUBROUTINE FFTRC  (A,N,X,IWK,WK)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            N,IWK(1)
      REAL*8             A(N),WK(1)
      COMPLEX*16         X(1)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            ND2P1,ND2,I,MTWO,M,IMAX,ND4,NP2,K,NMK,J
      REAL*8             RPI,ZERO,ONE,HALF,THETA,TP,G(2),B(2),Z(2),AI,
     1                   AR
      COMPLEX*16         XIMAG,ALPH,BETA,GAM,S1,ZD
      EQUIVALENCE        (GAM,G(1)),(ALPH,B(1)),(Z(1),AR),(Z(2),AI),
     1                   (ZD,Z(1))
      DATA               ZERO/0.0D0/,HALF/0.5D0/,ONE/1.0D0/,IMAX/24/
      DATA               RPI/3.141592653589793D0/
C                                  FIRST EXECUTABLE STATEMENT
      IF (N .NE. 2) GO TO 5
C                                  N EQUAL TO 2
      ZD = DCMPLX(A(1),A(2))
      THETA = AR
      TP = AI
      X(2) = DCMPLX(THETA-TP,ZERO)
      X(1) = DCMPLX(THETA+TP,ZERO)
      GO TO 9005
    5 CONTINUE
C                                  N GREATER THAN 2
      ND2 = N/2
      ND2P1 = ND2+1
C                                  MOVE A TO X
      J = 1
      DO 6 I=1,ND2
         X(I) = DCMPLX(A(J),A(J+1))
         J = J+2
    6 CONTINUE
C                                  COMPUTE THE CENTER COEFFICIENT
      GAM = DCMPLX(ZERO,ZERO)
      DO 10 I=1,ND2
         GAM = GAM + X(I)
   10 CONTINUE
      TP = G(1)-G(2)
      GAM = DCMPLX(TP,ZERO)
C                                  DETERMINE THE SMALLEST M SUCH THAT
C                                  N IS LESS THAN OR EQUAL TO 2**M
      MTWO = 2
      M = 1
      DO 15 I=1,IMAX
         IF (ND2 .LE. MTWO) GO TO 20
         MTWO = MTWO+MTWO
         M = M+1
   15 CONTINUE
   20 IF (ND2 .EQ. MTWO) GO TO 25
C                                  N IS NOT A POWER OF TWO, CALL FFTCC
      CALL FFTCC (X,ND2,IWK,WK)
      GO TO 30
C                                  N IS A POWER OF TWO, CALL FFT2C
   25 CALL FFT2C (X,M,IWK)
   30 ALPH = X(1)
      X(1) = B(1) + B(2)
      ND4 = (ND2+1)/2
      IF (ND4 .LT. 2) GO TO 40
      NP2 = ND2 + 2
      THETA = RPI/ND2
      TP = THETA
      XIMAG = DCMPLX(ZERO,ONE)
C                                  DECOMPOSE THE COMPLEX VECTOR X
C                                  INTO THE COMPONENTS OF THE TRANSFORM
C                                  OF THE INPUT DATA.
      DO 35 K = 2,ND4
         NMK = NP2 - K
         S1 = DCONJG(X(NMK))
         ALPH = X(K) + S1
         BETA = XIMAG*(S1-X(K))
         S1 = DCMPLX(DCOS(THETA),DSIN(THETA))
         X(K) = (ALPH+BETA*S1)*HALF
         X(NMK) = DCONJG(ALPH-BETA*S1)*HALF
         THETA = THETA + TP
   35 CONTINUE
   40 CONTINUE
      X(ND2P1) = GAM
 9005 RETURN
      END

