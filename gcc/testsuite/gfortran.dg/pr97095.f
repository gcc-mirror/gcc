! { dg-do compile }
! { dg-options "-O2 -ftree-vectorize" }
      subroutine gen3delem(nel,ial,ifix,xta,xnoref,dd,jact,nelshell)
      real*8 xnoref(3),xta(3,100),xn1(3,100)
      if(nel.gt.0) then
        do j=1,nel
        enddo
        do
        enddo
      endif
      do
        if(ifix.eq.0) then
          do j=nelshell,nel
            if(ial(j).eq.0) then
            endif
          enddo
        endif
        do j=nelshell,nel
        enddo
        do j=1,3
          xnoref(j)=xnoref(j)/dd
        enddo
        xn1(2,jact)=xnoref(3)*xta(1,jact)-xnoref(1)*xta(3,jact)
        xn1(3,jact)=xnoref(1)*xta(2,jact)-xnoref(2)*xta(1,jact)
        call foo(xn1)
      enddo
      end
