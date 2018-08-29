! { dg-do compile }
! { dg-options "-O2 -floop-nest-optimize" }
      SUBROUTINE STONG(IGAUSS)
      DIMENSION EXX(6)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),NSHELL
  100 CONTINUE
      NSHELL = NSHELL+1
      IF(NSHELL.GT.MXSH) THEN
         RETURN
      END IF
      DO 320 I = 1,IGAUSS
         K = K1+I-1
         EX(K) = EXX(I)*SCALE
  320 CONTINUE
      IF(TNORM.GT.TOLNRM) THEN
         STOP
      END IF
      DO 460 IG = K1,K2
         CS(IG) = FACS*CS(IG)
  460 CONTINUE
      GO TO 100
      END
