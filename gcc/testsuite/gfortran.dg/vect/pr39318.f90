! { dg-do compile { target fopenmp } } 
! { dg-options "-c -fopenmp -fexceptions -O2 -ftree-vectorize" } 

      subroutine adw_trajsp (F_u,i0,in,j0,jn)
      implicit none
      real F_u(*)
      integer i0,in,j0,jn
      integer n,i,j
      real*8 xsin(i0:in,j0:jn)
!$omp parallel do private(xsin)
         do j=j0,jn
         do i=i0,in
            xsin(i,j) = sqrt(F_u(n))
         end do
         end do
!$omp end parallel do
      return
      end

! { dg-final { cleanup-tree-dump "vect" } }

