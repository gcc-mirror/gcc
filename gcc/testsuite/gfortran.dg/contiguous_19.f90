! { dg-do compile }
!
! PR fortran/127613
! Check that simply contiguous polymorphic arrays are correctly recognized as
! such, and that the information correctly propagates to associate names.
! One associate name used as pointer association target below used to not be
! recognized as simply contiguous.

program prog
   implicit none
   integer, parameter :: n = 3
   type :: t
      integer :: c
   end type
   type(t), allocatable, target :: x(:,:)
   integer :: i
   x = reshape([ (t(i*i), i=1,n*n) ], [n,n])
   call sub1(x)
contains
   subroutine sub1(z)
      class(t), contiguous, target  :: z(:,:)
      class(t), contiguous, pointer :: p(:,:)
      class(t), pointer :: q(:)
      q(1:n*n) => z
      associate (a => z)
         q(1:n*n) => a
      end associate
      p => z
      q(1:n*n) => p
      associate (b => p)
         q(1:n*n) => b
      end associate
   end subroutine
end program
