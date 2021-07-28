c { dg-additional-options "-std=legacy -Wno-analyzer-use-of-uninitialized-value -Wno-analyzer-too-complex" }

      SUBROUTINE PPADD (A, C, BH)

      COMPLEX DD, FP, FPP, R1, R2
      DIMENSION A(*), C(*), BH(*)

      DO 136 IG=IS,1
         FP = (0.,0.)
         FPP = (0.,0.)

         DO 121 J=1,1
            DD = 1./2
            FP = DD
            FPP = DD+1
 121     CONTINUE

         R2 = -FP
         IF (ABS(R1)-ABS(R2)) 129,129,133
 129     R1 = R2/FPP
 133     IT = IT+1

 136  CONTINUE

      RETURN
      END
