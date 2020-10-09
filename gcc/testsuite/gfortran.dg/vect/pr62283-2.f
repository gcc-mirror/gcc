! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "-fdump-tree-slp2-details" }
      subroutine saxpy(alpha,x,y)
      real x(4),y(4),alpha
      y(1)=y(1)+alpha*x(1)
      y(2)=y(2)+alpha*x(2)
      y(3)=y(3)+alpha*x(3)
      y(4)=y(4)+alpha*x(4)
      end
! { dg-final { scan-tree-dump "optimized: basic block" "slp2" } }
