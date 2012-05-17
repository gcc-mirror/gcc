! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR 43969: [OOP] ALLOCATED() with polymorphic variables
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>


module foo_mod
  type foo_inner
    integer, allocatable :: v(:)
  end type foo_inner
  type foo_outer
    class(foo_inner), allocatable :: int
  end type foo_outer
contains
subroutine foo_checkit()
  implicit none
  type(foo_outer)    :: try
  type(foo_outer),allocatable :: try2
  class(foo_outer), allocatable :: try3
  
  if (allocated(try%int)) call abort()
  allocate(foo_outer :: try3)
  if (allocated(try3%int)) call abort()
  allocate(try2)
  if (allocated(try2%int)) call abort()
 
end subroutine foo_checkit
end module foo_mod


program main

  use foo_mod
  implicit none
  
  call foo_checkit()

end program main

! { dg-final { scan-tree-dump-times "__builtin_free" 11 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
