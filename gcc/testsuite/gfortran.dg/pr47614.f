! { dg-do run { target { powerpc*-*-* } } }
! { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } }
! { dg-options "-O3 -funroll-loops -ffast-math -mcpu=power4" }


      SUBROUTINE SFCPAR(ZET,NZ,ZMH,TSL,TMES)
      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 ZET(*)

      ZS=MAX(TSL*ZMH,ZET(2))

      DO 10 K=2,NZ
         KLEV=K-1
         IF(ZS.LE.ZET(K)) GO TO 20
 10   CONTINUE

 20   CONTINUE
      TMES=ZET(KLEV+1)
      
      RETURN
      END

      program pr47614
	real*8 ar1(10),d1,d2,d3
	integer i

	d1 = 2.0
	d2 = 3.0
	d3 = 3.0
	do 50 i=1,10
	  ar1(i) = d1
	  d1 = d1 + 2.0
 50	continue

	call sfcpar(ar1,10,d2,d3,d1)
	if (d1.ne.10.0) call abort()
      end
