c { dg-do compile }
c { dg-additional-options "-std=legacy" }
      subroutine derv (xx,b,bv,det,r,s,t,ndopt,cosxy,thick,edis,
     1                  vni,vnt)
      implicit real*8 (a-h,o-z)
      save
c
      common /shell1/ disd(9),ield,ielp,npt,idw,ndrot
      common /shell4/xji(3,3),p(3,32),h(32)
c
      dimension xx(3,*),ndopt(*),bv(*),vni(*),cosxy(6,*),vnt(*),
     1          edis(*),thick(*),b(*)
c
      kk=0
      k2=0
      do 130 k=1,ield
      k2=k2 + 3
      if (ndopt(k)) 127,127,130
  127 kk=kk + 1
      do 125 i=1,3
      b(k2+i)=b(k2+i) + (xji(i,1)*p(1,k) + xji(i,2)*p(2,k))*t
     1         + xji(i,3)*h(k)
      th=0.5*thick(kk)
      b(k2+i+3)=b(k2+i+3) - th*cosxy(i+3,kk)
  125 b(k2+i+6)=b(k2+i+6) + th*cosxy(i,kk)
      k2=k2 + 9
  130 continue
      return
      end
