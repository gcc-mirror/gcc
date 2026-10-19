! { dg-do run }
!
! PR fortran/127637
! Check that the scalarization of the assignment picks the loop upper bound from
! the non-optional array.

program prog
   implicit none
   integer, parameter :: n = 5
   type :: t
      integer :: c
   end type
   type(t), allocatable :: x(:)
   integer :: i
   call sub()
contains
   elemental function f(v, a) result(r)
      integer, intent(in) :: v
      type(t), optional, intent(in) :: a
      integer :: r
      r = v + 42
      if (present(a)) r = a%c
   end function
   subroutine sub(o)
      class(t), optional, intent(in) :: o(:)
      integer, allocatable :: y(:)
      allocate(y(n))
      y = 0
      y(:) = f(y, o)
      if (any(y /= 42)) error stop 1
   end subroutine
end program
