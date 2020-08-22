! { dg-do compile }
      subroutine deuldlag(xi,et,ze,xlag,xeul,xj,xs)
      real*8 shp(3,20),xs(3,3),xlag(3,20),xeul(3,20)
      do i=1,3
        do j=1,3
        enddo
      enddo
      do i=1,3
        do j=1,3
          xs(i,j)=0.d0
          do k=1,20
            xs(i,j)=xs(i,j)+xeul(i,k)*shp(j,k)
          enddo
        enddo
      enddo
      end
