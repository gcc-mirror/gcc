! { dg-do compile }
! { dg-options "-O3" }
      implicit real*8 (a-h,o-z)
      common clop6(3),dps(6),aml6(6,6)
      dimension y1(3)
      dimension dclo(3)
      dimension dx(3),dy(3)
      save
      do 80 ii=1,itco
        y1(3)=dps(1)
        do 40 l=1,3
          dy(l)=clop6(l)-y1(l)
   40   continue
        dczp=abs(dy(3))
        if(dcx.le.c1m10.and.dcz.le.c1m10.and.dcxp.le.c1m10.and.dczp
     +  .le.c1m10.and.dcy.le.c1m10.and.dcyp.le.c1m10) goto 90
   80 continue
      write(6) itco
      ii=itco
   90 continue
      if(ii.ne.itco) then
        do 65 k=1,3
          do 55 j=1,3
            jj=2*j
            kk=2*k
            dclo(k)=aml6(kk-1,jj-1)*dx(j)+dclo(k)
            dclo(k)=aml6(kk-1,jj)*dy(j)+dclo(k)
   55     continue
   65   continue
      endif
      end

