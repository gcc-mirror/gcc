! { dg-do compile }
! { dg-require-effective-target vect_float }

subroutine s111 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc) 
!     linear dependence testing
!     no dependence - vectorizable
!     but not consecutive access

      integer ntimes, ld, n, i, nl
      real a(n), b(n), c(n), d(n), e(n), aa(ld,n), bb(ld,n), cc(ld,n)
      real t1, t2, second, chksum, ctime, dtime, cs1d
      do 1 nl = 1,2*ntimes
      do 10 i = 2,n,2
         a(i) = a(i-1) + b(i)
  10  continue
      call dummy(ld,n,a,b,c,d,e,aa,bb,cc,1.)
  1   continue
      return
      end

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" } }
! { dg-final { scan-tree-dump-times "complicated access pattern" 1 "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }

