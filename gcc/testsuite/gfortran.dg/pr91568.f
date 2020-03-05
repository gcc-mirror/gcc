! { dg-do compile }
! { dg-options "-Ofast" }
      subroutine h3dall(z,hvec,hder,nterms)
      complex *16 hvec(0:1),hder(0:1)
      complex *16 z,zinv,ztmp/1.0/
      zinv=1.0/z
      do i=1,nterms
         ztmp=zinv*i
         hder(i)=hvec(i-1)-ztmp*hvec(i)
      enddo
      end
