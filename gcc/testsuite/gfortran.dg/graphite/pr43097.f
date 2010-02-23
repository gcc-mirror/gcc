! { dg-options "-O2 -fgraphite-identity" }

      subroutine foo (ldmx,ldmy,nx,ny,v)
      implicit real*8 (a-h, o-z)
      dimension v(5,ldmx,ldmy,*)
      dimension tmat(5,5)

      k = 2
      do j = 2, ny-1
         do i = 2, nx-1
            do ip = 1, 4
               do m = ip+1, 5
                  v(m,i,j,k) = v(m,i,j,k) * m
               end do
            end do
            do m = 5, 1, -1
               do l = m+1, 5
                  v(m,i,j,k) = v(l,i,j,k)
               end do
               v(m,i,j,k) = m
           end do
         end do
      end do
      return
      end
