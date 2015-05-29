! { dg-do compile }
      subroutine foo(a,c,i,m)
      real a(4,*),b(3,64),c(3,200),d(64)
      integer*8 i,j,k,l,m
      do j=1,m,64
        do k=1,m-j+1
          d(k)=a(4,j-1+k)
          do l=1,3
            b(l,k)=c(l,i)+a(l,j-1+k)
          end do
        end do
        call bar(b,d,i)
      end do
      end


