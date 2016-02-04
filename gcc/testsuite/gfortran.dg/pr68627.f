! { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }

! { dg-options "-Ofast -mavx512f -ffixed-xmm1 -ffixed-xmm2 -ffixed-xmm3 -ffixed-xmm4 -ffixed-xmm5 -ffixed-xmm6 -ffixed-xmm7 -ffixed-xmm8 -ffixed-xmm9 -ffixed-xmm10 -ffixed-xmm11 -ffixed-xmm12 -ffixed-xmm13 -ffixed-xmm14 -ffixed-xmm15" }

      IMPLICIT REAL*8(A-H,O-Z)
      ALLOCATABLE DD1(:), DD2(:), WY(:,:)
      ALLOCATE( DD1(MAX), DD2(MAX), WY(MAX,MAX))
         DO J = J1,J2
            DO I = I1, I2
               DD1(I) = D1 * (WY(I-2,J) - WY(I+2,J) +
     >              (WY(I+1,J) - WY(I-1,J)))
            END DO
            DO I = I1, INT(D2 * D3(I))
            END DO
         END DO
      END

! { dg-final { scan-assembler-not "vbroadcastsd\[ \\t\]+%xmm\[0-9\]+, %ymm\[0-9\]+" } }
