! { dg-do compile }
! { dg-options "-Ofast -frounding-math" }
      SUBROUTINE PASSB4 (CC,CH)
      DIMENSION CC(IDO,4,L1), CH(IDO,L1,*)
         DO 103 I=2,IDO,2
            TI4 = CC0-CC(I,4,K)
            CI4 = TI1-TI4
            CH(I-1,K,4) = CI4
            CH(I,K,4) = CI4
  103    CONTINUE
      END
