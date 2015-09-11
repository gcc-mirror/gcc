      dimension p1(2),t(6,4),b1(2),b2(2),al1(2),al2(2),g1(2),g2(2)
      save
      if(nlin.eq.0) then
        do 20 l=1,2
          ll=2*l
          b2(l)=t(6-ll,ll-1)*t(6-ll,ll-1)+t(7-ll,ll-1)*t(7-ll,ll-1)
          write(*,*) b2(l)
   20   continue
      endif
      end

! { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite" { xfail *-*-* } } } 
