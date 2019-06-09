! { dg-do run }
!
! Test the fix for PR57284 - [OOP] ICE with find_array_spec for polymorphic
! arrays. Once thw ICE was fixed, work was needed to fix a segfault while
! determining the size of 'z'.
!
! Contributed by Lorenz Huedepohl  <bugs@stellardeath.org>
!
module testmod
  type type_t
    integer :: idx
  end type type_t
  type type_u
     type(type_t), allocatable :: cmp(:)
  end type
contains
  function foo(a, b) result(add)
    class(type_t), intent(in) :: a(:), b(size(a))
    type(type_t) :: add(size(a))
    add%idx = a%idx + b%idx
  end function
end module testmod
program p
  use testmod
  class(type_t), allocatable, dimension(:) :: x, y, z
  class(type_u), allocatable :: w
  allocate (x, y, source = [type_t (1), type_t(2)])
  z = foo (x, y)
  if (any (z%idx .ne. [2, 4])) stop 1

! Try something a bit more complicated than the original.

  allocate (w)
  allocate (w%cmp, source = [type_t (2), type_t(3)])
  z = foo (w%cmp, y)
  if (any (z%idx .ne. [3, 5])) stop 2
  deallocate (w, x, y, z)
end program
