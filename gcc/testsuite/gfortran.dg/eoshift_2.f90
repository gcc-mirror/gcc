! { dg-do compile }
! pr35724 compile time segmentation fault for eoshift with negative third arg
subroutine ra0072(dda,lda,nf10,nf1,mf1,nf2)
      real dda(10,10)
      logical lda(10,10)
      dda = eoshift(dda,(/mf1,nf1/),tws0r,nf3-nf1)
      lda = cshift(lda,(/mf1,nf1/),nf3-nf1)
      where (lda) dda = eoshift(dda,1,1.0,-mf1)
end subroutine
