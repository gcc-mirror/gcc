! { dg-do run }
! { dg-require-effective-target vect_double }
! { dg-require-effective-target vect_intdouble_cvt }
! { dg-additional-options "-fno-inline -ffast-math" }
      subroutine foo(a,x,y,n)
      implicit none
      integer n,i

      real*8 y(n),x(n),a

      do i=1,n
         a=a+x(i)*y(i)+x(i)
      enddo

      return
      end

      program test
      real*8 x(1024),y(1024),a
      do i=1,1024
        x(i) = i
        y(i) = i+1
      enddo
      call foo(a,x,y,1024)
      if (a.ne.359488000.0) STOP 1
      end
! If there's no longer a reduction chain detected this doesn't test what
! it was supposed to test, vectorizing a reduction chain w/o SLP.
! { dg-final { scan-tree-dump "reduction chain" "vect" } }
! We should vectorize the reduction in foo and the induction in test.
! { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } }
