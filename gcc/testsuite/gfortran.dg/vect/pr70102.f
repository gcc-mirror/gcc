! { dg-do compile }
! { dg-additional-options "-Ofast" }
      subroutine test (x,y,z)
      integer x,y,z
      real*8 a(5,x,y,z),b(5,x,y,z)
      real*8 c

      c = 0.0d0
      do k=1,z
         do j=1,y
           do i=1,x
              do l=1,5
                 c = c + a(l,i,j,k)*b(l,i,j,k)
              enddo
           enddo
         enddo
      enddo
      write(30,*)'c ==',c
      return
      end
! { dg-final { scan-tree-dump "vectorizing a reduction chain" "vect" { target vect_double } } }
