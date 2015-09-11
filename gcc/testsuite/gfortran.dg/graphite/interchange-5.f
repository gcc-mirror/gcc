      subroutine s235 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
c
c     loop interchanging
c     imperfectly nested loops
c
      integer ntimes, ld, n, i, nl, j
      double precision a(n), b(n), c(n), d(n), e(n), aa(ld,n),
     +                 bb(ld,n), cc(ld,n)
      double precision chksum, cs1d, cs2d
      real t1, t2, second, ctime, dtime

      call init(ld,n,a,b,c,d,e,aa,bb,cc,'s235 ')
      t1 = second()
      do 1 nl = 1,ntimes/n
      do 10 i = 1,n
         a(i) =  a(i) + b(i) * c(i)
         do 20 j = 2,n
            aa(i,j) = aa(i,j-1) +  bb(i,j) * a(i)
  20     continue
  10  continue
      call dummy(ld,n,a,b,c,d,e,aa,bb,cc,1.d0)
  1   continue
      t2 = second() - t1 - ctime - ( dtime * float(ntimes/n) )
      chksum = cs2d(n,aa) + cs1d(n,a)
      call check (chksum,(ntimes/n)*n*(n-1),n,t2,'s235 ')
      return
      end

! { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" { xfail *-*-* } } }
