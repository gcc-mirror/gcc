! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR83763 in which a dependency was not handled correctly, which
! resulted in a runtime segfault.
!
! Contributed by Berke Durak  <berke.durak@gmail.com>
!
module bar
  implicit none

  type :: foo(n)
     integer, len :: n = 10
     real :: vec(n)
  end type foo

contains

  function baz(a) result(b)
    type(foo(n = *)), intent(in) :: a
    type(foo(n = a%n)) :: b

    b%vec = a%vec * 10
  end function baz

end module bar

program test
  use bar
  implicit none
  call main1   ! Original report
  call main2   ! Check for memory loss with allocatable 'x' and 'y'.

contains

  subroutine main1
    type(foo(5)) :: x, y
    integer :: a(5) = [1,2,3,4,5]

    x = foo(5)(a)
    x = baz (x)            ! Segmentation fault because dependency not handled.
    if (any (x%vec /= 10 * a)) stop 1
    y = x
    x = baz (y)            ! No dependecy and so this worked.
    if (any (x%vec /= 100 * a)) stop 2
  end subroutine main1

  subroutine main2
    type(foo(5)), allocatable :: x, y
    integer :: a(5) = [1,2,3,4,5]

    x = foo(5)(a)
    x = baz (x)            ! Segmentation fault because dependency not handled.
    if (any (x%vec /= 10 * a)) stop 3
    y = x
    x = baz (y)            ! No dependecy and so this worked.
    if (any (x%vec /= 100 * a)) stop 4
  end subroutine main2

end program test
! { dg-final { scan-tree-dump-times "__builtin_free" 16 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_malloc" 12 "original" } }
