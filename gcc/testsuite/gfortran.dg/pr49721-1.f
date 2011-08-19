! PR middle-end/49721
! { dg-do compile }
! { dg-options "-O3 -funroll-loops" }

      subroutine midbloc6(c,a2,a2i,q)
      parameter (ndim2=6)
      parameter (ndim=3)
      dimension ri(ndim2),cr(ndim2,ndim2),xj(ndim2,ndim2),q(*)
     @,sai(ndim2,ndim2),cm(ndim2,ndim2),w(ndim2,ndim2)
      dimension vr(ndim2,ndim2),vi(ndim2,ndim2),s1(ndim2,ndim2),p(ndim)
      dimension xq(6),qb(2),qc(2),ifl(6),iplane(3)
      save
      call eig66(cr,rr,ri,vr,vi)
      xq(i)=asin(ri(i))/x2pi
      i9=6
      qb(1)=q(1)/x2pi
        do 180 i=1,2
          do 170 j=1,6
  120       if(xq(j)) 130,190,140
  130       if(qb(i)-0.5d0) 160,150,150
  140       if(qb(i)-0.5d0) 150,150,160
  150       continue
            tst=abs(abs(qb(i))-abs(xq(j)))
  160       continue
  170     continue
          iplane(i)=k
  180   continue
  190   continue
      n1=iplane(3)
      if(i9.eq.6) then
        z=vr(1,n1)*vi(2,n1)-vr(2,n1)*vi(1,n1)+vr(3,n1)*vi(4,n1)-vr(4,n1)
      endif
      sai(6,i)=vi(i,n1)/z
      call dacond6(a2,zero)
      end
