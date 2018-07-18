! { dg-do compile }
! { dg-options "-O2 -floop-nest-optimize -std=legacy" }

      SUBROUTINE JDFIDX(MKL,KGSH)
      DIMENSION MKL(6,6)
      NKL=0
  400 DO 40 KG = 1,KGSH
      DO 40 LG = 1,KG
      NKL = NKL + 1
   40 MKL(LG,KG) = NKL
      END
