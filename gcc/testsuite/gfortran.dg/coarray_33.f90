! { dg-do compile }
! { dg-options "-fcoarray=lib" }
type t
  integer :: x
end type t

class(t), allocatable :: a[:]
allocate(t :: a[*])
a%x = this_image()

call foo(a[i]) ! { dg-error "Coindexed polymorphic actual argument at .1. is passed polymorphic dummy argument" }
contains
subroutine foo(y)
  class(t) :: y
  print *, y%x
end subroutine foo
end
