! { dg-do compile }
      subroutine ice(npoint, nterm, x, g)
      implicit none
      integer    norder
      parameter (norder=10)
      integer j
      integer k
      integer ii
      integer nterm
      integer npoint
      real b(norder)
      real c(norder)
      real d(norder)
      real x(npoint)
      real g(npoint)
      real gg
      real prev
      real prev2

          j = 1
    100   continue
          j = j+1
          if (nterm == j)  then
             do ii=1,npoint
                k = nterm
                gg= d(k)
                prev= 0.0
                do k=k-1,1,-1
                   prev2= prev
                   prev= gg
                   gg = d(k)+(x(ii)-b(k))*prev-c(k+1)*prev2
                enddo
                g(ii) = gg
             enddo
          endif
          go to 100
      end
