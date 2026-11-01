! { dg-do run }
!
! PR fortran/127827
! Check that array access correctly uses the span from the descriptor,
! including for polymorphic dummy arguments.

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
   a = [ (t2(3*i*i+2, i*i+1, 5*i+3), i=1,size(a)) ]
   call check_all(a%t1, 1)
   call type_wrapper(a%t1, 2, 3)
   call class_wrapper(a%t1, 4)
contains
   subroutine type_wrapper(x, e1, e2)
      type(t1), target, intent(in) :: x(:)
      integer, intent(in) :: e1, e2
      call check_all(x, e1)
      call class_wrapper(x, e2)
   end subroutine
   subroutine class_wrapper(x, e)
      class(t1), target, intent(in) :: x(:)
      integer, intent(in) :: e
      call check_all(x, e)
   end subroutine
   subroutine check_all(x, e)
      type(t1), target, intent(in) :: x(:)
      integer, intent(in) :: e
      call check(x%c1, [ 5, 14, 29, 50, 77 ], e*10+1)
      call check(x%c2, [ 2,  5, 10, 17, 26 ], e*10+2)
   end subroutine
   subroutine check(a, v, e)
      integer, intent(in), dimension(:) :: a, v
      integer, intent(in) :: e
      integer :: i
      if (size(a) /= size(v)) error stop e*10
      !do i = 1, size(a)
      !   print *, e*10+i, (a(i) == v(i) ? "PASS" : "FAIL"), a(i), v(i)
      !end do
      if (any(a /= v)) error stop e*10+9
   end subroutine
end program
