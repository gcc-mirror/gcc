! { dg-do compile }
! PR35724 Compile time segmentation fault for CSHIFT with negative third arg
      SUBROUTINE RA0072(DDA,LDA,nf10,nf1,mf1,nf2)
      REAL DDA(10,10)
      LOGICAL LDA(10,10)
      WHERE (LDA) DDA = CSHIFT(DDA,1,-MF1)  ! MF1 works, -1 works
      END SUBROUTINE

