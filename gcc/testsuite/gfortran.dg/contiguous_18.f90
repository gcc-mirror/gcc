! { dg-do compile }
!
! PR fortran/127701
! Check that a simply contiguous reference is correctly recognized as such when
! it is a subreference of an associate name.

program prog
   implicit none
   integer, parameter :: l = 7
   integer, parameter :: m = 5
   integer, parameter :: s = 2
   integer, parameter :: n = (m + s - 1) / s
   type :: t1
      integer :: c1(l,l)
   end type
   type(t1), target :: x(m)
   integer, target :: y(m,m)
   integer :: i, j
   x = (/ (t1(reshape([ (i+j, j=1,l*l) ], [l,l])), i=1,size(x,1)) /)
   call sub1(x(::s))
   y = reshape((/ (i*i, i=1,size(y)) /), shape(y))
   call sub2(y(::s,:))
   call sub3(y)
contains
   subroutine sub1(a)
      type(t1), target :: a(:)
      integer, contiguous, pointer :: p(:)
      associate(b => a)
         p(1:l*l) => b(1)%c1  ! no error
      end associate
   end subroutine
   subroutine sub2(a)
      integer, target :: a(:,:)
      integer, contiguous, pointer :: p(:)
      associate(b => a)
         p(1:n*m) => b(:,:)   ! { dg-error "must be rank 1 or simply contiguous" }
      end associate
   end subroutine
   subroutine sub3(a)
      integer, target :: a(m,m)
      integer, contiguous, pointer :: p(:)
      associate(b => a)
         p(1:n*m) => b(::s,:) ! { dg-error "must be rank 1 or simply contiguous" }
      end associate
   end subroutine
end program
