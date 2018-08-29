! { dg-do compile }
! { dg-additional-options "-ffast-math -ftree-vectorize" }
      SUBROUTINE SOBOOK(MHSO,HSOMAX,MS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX*16 HSOT,HSO1(2)
      PARAMETER (ZERO=0.0D+00,TWO=2.0D+00)
      DIMENSION SOL1(3,2),SOL2(3)
      CALL FOO(SOL1,SOL2)
      SQRT2=SQRT(TWO)
      DO IH=1,MHSO
        IF(MS.EQ.0) THEN
          HSO1(IH) =  DCMPLX(ZERO,-SOL1(3,IH))
          HSOT =  DCMPLX(ZERO,-SOL2(3))
        ELSE
          HSO1(IH) =  DCMPLX(-SOL1(2,IH),SOL1(1,IH))/SQRT2
          HSOT =  DCMPLX(-SOL2(2),SOL2(1))/SQRT2
        ENDIF
      ENDDO
      HSOT=HSOT+HSO1(1)
      HSOMAX=MAX(HSOMAX,ABS(HSOT))
      RETURN
      END
