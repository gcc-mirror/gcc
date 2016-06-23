! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-additional-options "-O3 -march=core2 -mavx -ffast-math -mveclibabi=svml" }

      integer index(18),i,j,k,l,ipiv(18),info,ichange,neq,lda,ldb,
     &  nrhs,iplas
      real*8 ep0(6),al10(18),al20(18),dg0(18),ep(6),al1(18),
     &  al2(18),dg(18),ddg(18),xm(6,18),h(18,18),ck(18),cn(18),
     &  c(18),d(18),phi(18),delta(18),r0(18),q(18),b(18),cphi(18),
     &  q1(18),q2(18),stri(6),htri(18),sg(18),r(42),xmc(6,18),aux(18),
     &  t(42),gl(18,18),gr(18,18),ee(6),c1111,c1122,c1212,dd,
     &  skl(3,3),xmtran(3,3),ddsdde(6,6),xx(6,18)
      do
         do i=1,18
            htri(i)=dabs(sg(i))-r0(i)-ck(i)*(dg(i)/dtime)**(1.d0/cn(i))
            do j=1,18
            enddo
         enddo
         do
                  if(i.ne.j) then
                  gr(index(i),1)=htri(i)
               endif
            call dgesv(neq,nrhs,gl,lda,ipiv,gr,ldb,info)
         enddo
      enddo
      end

