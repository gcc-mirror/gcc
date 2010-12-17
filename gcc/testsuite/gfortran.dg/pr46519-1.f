! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-options "-O3 -mavx -mvzeroupper -mtune=generic -dp" }

      PROGRAM MG3XDEMO 
      INTEGER LM, NM, NV, NR, NIT


      PARAMETER( LM=7 )
C      PARAMETER( NIT=40 )
      PARAMETER( NM=2+2**LM, NV=NM**3 )
      PARAMETER( NR = (8*(NM**3+NM**2+5*NM-23+7*LM))/7 )
C
C
C If commented line is used than there is no penalty
C	COMMON /X/ U, V, R, A, C, IR, MM
	COMMON /X/ A, C, IR, MM
      REAL*8 A(0:3),C(0:3)

      INTEGER IT, N
      INTEGER LMI, MTIME, NTIMES
C
      READ *,LMI
      READ *,NIT
      READ *,NTIMES
      READ *,U0

      READ 9004, A
      READ 9004, C
9004  FORMAT (4D8.0)

      DO I = 0, 3
	A(I) = A(I)/3.0D0
	C(I) = C(I)/64.0D0
      ENDDO
C
      N  = 2 + 2**LMI

      WRITE(6,7)N-2,N-2,N-2,NIT
 6    FORMAT( I4, 2E19.12)
 7    FORMAT(/,' KERNEL B:  SOLVING A POISSON PROBLEM ON A ',I6,' BY ',
     > I6,' BY ',I6,' GRID,',/,' USING ',I6,' MULTIGRID ITERATIONS.',/)
C
      STOP
      END

! { dg-final { scan-assembler-times "avx_vzeroupper" 1 } }
