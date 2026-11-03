! { dg-do run }
!
! PR fortran/PR128349
! Check that to evaluate the TRANSFER intrinsic call, the copy to a temporary
! array is done using the polymorphic copy function instead of a regular
! assignment of void pointer values.

program prog
   implicit none
   integer, parameter :: n = 5
   type :: t1
      integer :: c1, c2, c3
   end type
   type :: t2
      integer :: c1, c2, c3
   end type
   type(t1) :: a(n)
   integer :: i
   a = [ (t1(2*i+1, 3*i*i+2, 5*i*i+3), i=1, n) ]
   call sub(a)
contains
   subroutine sub(x)
      class(*), intent(in) :: x(:)
      type(t2) :: y(n)
      call check_t2(transfer(x, y))
   end subroutine
   subroutine check_t2(x)
      type(t2), target, intent(in) :: x(:)
      call check(x%c1, [3,  5,  7,  9,  11], 1)
      call check(x%c2, [5, 14, 29, 50,  77], 2)
      call check(x%c3, [8, 23, 48, 83, 128], 3)
   end subroutine
   subroutine check(x, y, e)
      integer, intent(in) :: x(:), y(:), e
      if (size(x) /= size(y)) error stop e*10+8
      !do i=1,size(x)
      !   print *, e*10+i, (x(i) == y(i) ? "PASS" : "FAIL"), x(i), y(i)
      !end do
      if (any(x /= y)) error stop e*10+9
   end subroutine
end program
