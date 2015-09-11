! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 59589: [4.9 Regression] [OOP] Memory leak when deallocating polymorphic
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

  implicit none

  type :: foo
     real, allocatable :: x(:)
  end type

  type :: bar
     type(foo) :: f
  end type

  class(bar), allocatable :: b

  allocate(bar::b)
  allocate(b%f%x(1000000))
  b%f%x = 1.
  deallocate(b)

end

! { dg-final { scan-tree-dump-times "__builtin_free" 4 "original" } }
