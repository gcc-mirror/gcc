! PR 77848: Verify versioning is on when vectorization fails
! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-ifcvt -fdump-tree-vect-details" }

      subroutine sub(x,a,n,m)
      implicit none
      real*8 x(*),a(*),atemp
      integer i,j,k,m,n
      real*8 s,t,u,v
      do j=1,m
         atemp=0.d0
         do i=1,n
            if (abs(a(i)).gt.atemp) then
               atemp=a(i)
               k = i
            end if
         enddo
         call dummy(atemp,k)
      enddo
      return
      end

! { dg-final { scan-tree-dump "LOOP_VECTORIZED" "ifcvt" } }
! { dg-final { scan-tree-dump "vectorized 0 loops in function" "vect" } }
