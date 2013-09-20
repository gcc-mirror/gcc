! { dg-do compile }
! { dg-options "-O2" }
      SUBROUTINE UMPSE(AIBJ,NOC,NDIM,NOCA,NVIRA,NOCCA,E2)
      DIMENSION AIBJ(NOC,NDIM,*)
      DO 20 MA=1,NVIRA
      DO 20 MI=1,NOCA
         DO 10 MB=1,MA
         MBI = MI+NOCA*(MB-1)
         DO 10 MJ=1,NOCCA
            DUM = AIBJ(MJ,MAI,MB)-AIBJ(MJ,MBI,MA)
            E2A = E2A-DUM
   10    CONTINUE
   20 CONTINUE
      E2  = E2+E2A
      END
