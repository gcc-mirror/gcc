! { dg-do compile }
! { dg-options "-O3" }
      subroutine hesfcn(n, x, h, ldh)
      integer n,ldh
      double precision x(n), h(ldh)

      integer i,j,k,kj
      double precision th,u1,u2,v2
 
      kj = 0
      do 770 j = 1, n
         kj = kj - j
         do 760 k = 1, j
            kj = kj + 1
            v2 = 2 * x(k) - 1
            u1 = 0
            u2 = 2
            do 750 i = 1, n
               h(kj) = h(kj) + u2
               th = 4 * v2 + u2 - u1
               u1 = u2
               u2 = th
               th = v2 - 1
  750       continue
  760    continue
  770 continue

      end
