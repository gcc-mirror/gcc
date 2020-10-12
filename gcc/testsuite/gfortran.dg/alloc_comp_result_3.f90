! { dg-do run }
!
! Test the fix for PR96495 - segfaults at runtime at locations below.
!
! Contributed by Paul Luckner  <paul.luckner@rwth-aachen.de>
!
module foo_m

  implicit none

  type foo
    integer, allocatable :: j(:)
  end type

  interface operator(.unary.)
    module procedure neg_foo
  end interface

  interface operator(.binary.)
    module procedure foo_sub_foo
  end interface

  interface operator(.binaryElemental.)
    module procedure foo_add_foo
  end interface

contains

  elemental function foo_add_foo(f, g) result(h)
    !! an example for an elemental binary operator
    type(foo), intent(in) :: f, g
    type(foo)             :: h

    allocate (h%j(size(f%j)), source = f%j+g%j)
  end function

  elemental function foo_sub_foo(f, g) result(h)
    !! an example for an elemental binary operator
    type(foo), intent(in) :: f, g
    type(foo)             :: h

    allocate (h%j(size(f%j)), source = f%j-3*g%j)
  end function

  pure function neg_foo(f) result(g)
    !! an example for a unary operator
    type(foo), intent(in) :: f
    type(foo)             :: g

    allocate (g%j(size(f%j)), source = -f%j)
  end function

end module

program main_tmp

  use foo_m

  implicit none

  type(foo) f, g(2)

  allocate (f%j(3))
  f%j = [2, 3, 4]

  g = f
  if (any (g(2)%j .ne. [2, 3, 4])) stop 1

  g = g .binaryElemental. (f .binary. f)     ! threw "Segmentation fault"
  if (any (g(2)%j .ne. [-2,-3,-4])) stop 2

  g = g .binaryElemental. (  .unary.  f)     ! threw "Segmentation fault"
  if (any (g(2)%j .ne. [-4,-6,-8])) stop 3

end program
