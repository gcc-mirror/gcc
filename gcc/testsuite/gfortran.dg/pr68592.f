! PR tree-optimization/68592
! { dg-do compile }
! { dg-require-profiling "-fprofile-generate" }
! { dg-options "-Ofast -fprofile-generate" }
! { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } }
      PARAMETER (MXCPGA=320,ZERO=0.0)
      DIMENSION CPNORM(MXCPGA),CDNORM(MXCPGA),
     *          CFNORM(MXCPGA)  
         KTYPIL        = KTYPI()
         DO 84 K=1,NOGTF
           LMP=LMP+1
           CFNORM(LMP)=ZERO
           IF (KTYPIL.EQ.1) LMP=CMPILMP
           IF (KTYPIL.EQ.2) CPNORM(LMP)=CMPILMP
           IF (KTYPIL.EQ.3) CDNORM(LMP)=CMPILMP
           IF (KTYPIL.EQ.4) LMP=CMPILMP
           IF (KTYPIL.EQ.6) LMP=CMPILMP
   84    CONTINUE
         CALL MMPNOR(CPNORM,CDNORM,CFNORM) 
      END
