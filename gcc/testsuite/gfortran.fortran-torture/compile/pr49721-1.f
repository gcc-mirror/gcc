       PARAMETER( LM=7 )
      PARAMETER( NM=2+2**LM, NV=NM**3 )
      PARAMETER( NR = (8*(NM**3+NM**2+5*NM-23+7*LM))/7 )
	COMMON /X/ U, V, R, A
      REAL*8 U(NR),V(NV),R(NR),A(0:3)
      DO 20 IT=1,NIT
        CALL RESID(U,V,R,N,A)
 20   CONTINUE
      END
