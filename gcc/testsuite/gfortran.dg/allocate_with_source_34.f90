! { dg-do run }
!
! PR fortran/128033
! Check that array allocation from polymorphic pointer source correctly
! implements the array copy.

program prog
   implicit none
   integer, parameter :: n = 5
   type :: t1
      integer :: c1, c2
   end type
   type, extends(t1) :: t2
      integer :: c3
   end type
   type(t2), target :: a(n)
   integer :: i
   a = [ (t2(2*i*i+3, i*i+2, 3*i+5), i=1,n) ]
   call sub(a)
contains
   subroutine sub(x)
      class(t1), pointer, intent(in) :: x(:)
      class(t1), allocatable :: a(:)
      allocate(a, source=x)
      call check_all(x, 1)
      call check_all(a, 2)
   end subroutine
   subroutine check_all(x, e)
      class(t1), target, intent(in) :: x(:)
      integer, intent(in) :: e
      select type(x)
         type is (t2)
            call check(x%c1, [ 5, 11, 21, 35, 53], e*10+1)
            call check(x%c2, [ 3,  6, 11, 18, 27], e*10+2)
            call check(x%c3, [ 8, 11, 14, 17, 20], e*10+3)
         class default
            error stop e*10+7
      end select
   end subroutine
   subroutine check(x, y, e)
      integer, target, dimension(:), intent(in) :: x, y
      integer, intent(in) :: e
      character(4) :: status
      integer :: i
      if (size(x) /= size(y)) error stop e*10+8
      if (any(x /= y)) error stop e*10+9
   end subroutine
end program
