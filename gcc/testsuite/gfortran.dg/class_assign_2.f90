! { dg-do link }
!
! PR 86484:[OOP] Undefined symbol when using polymorphic intrinsic assignment
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

program test_assign

  implicit none

  type :: foo_t
  end type

  type, extends (foo_t) :: bar_t
  end type

  class(foo_t), allocatable :: f
  type(bar_t)               :: b

  f = b

end
