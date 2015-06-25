! { dg-do compile }
! { dg-additional-options "-Ofast" }
! { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } }

      SUBROUTINE PASSB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      IMPLICIT REAL(4) (A-H, O-Z)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,&
                     WA1(*)     ,WA2(*)     ,WA3(*)
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,4,K)-CC(I,2,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,2,K)-CC(I-1,4,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CI4 = TI1-TI4
            CH(I-1,K,2) = TI1
            CH(I,K,2) = CR2
            CH(I-1,K,3) = WA2(I-1)*CR3-WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3+WA2(I)*CR3
            CH(I-1,K,4) = CI4
            CH(I,K,4) = CI4
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
