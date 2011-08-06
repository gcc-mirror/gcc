! { dg-do compile }
! { dg-require-effective-target vect_double }
      subroutine shell(nx,ny,nz,q,dq)
      implicit none
      integer i,j,k,l,nx,ny,nz
      real*8 q(5,nx,ny),dq(5,nx,ny)
         do j=1,ny
            do i=1,nx
               do l=1,5
                  q(l,i,j)=q(l,i,j)+dq(l,i,j)
               enddo
            enddo
         enddo
      return
      end
! { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail vect_no_align } } }
! { dg-final { cleanup-tree-dump "vect" } }
