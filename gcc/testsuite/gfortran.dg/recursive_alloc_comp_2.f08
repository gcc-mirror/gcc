! { dg-do run }
!
! Tests functionality of recursive allocatable derived types.
!
module m
  type :: recurses
    type(recurses), allocatable :: left
    type(recurses), allocatable :: right
    integer, allocatable :: ia
  end type
contains
! Obtain checksum from "keys".
  recursive function foo (this) result (res)
    type(recurses) :: this
    integer :: res
    res = this%ia
    if (allocated (this%left)) res = res + foo (this%left)
    if (allocated (this%right)) res = res + foo (this%right)
  end function
! Return pointer to member of binary tree matching "key", null otherwise.
  recursive function bar (this, key) result (res)
    type(recurses), target :: this
    type(recurses), pointer :: res
    integer :: key
    if (key .eq. this%ia) then
      res => this
      return
    else
      res => NULL ()
    end if
    if (allocated (this%left)) res => bar (this%left, key)
    if (associated (res)) return
    if (allocated (this%right)) res => bar (this%right, key)
  end function
end module

  use m
  type(recurses), allocatable, target :: a
  type(recurses), pointer :: b => NULL ()

! Check chained allocation.
  allocate(a)
  a%ia = 1
  allocate (a%left)
  a%left%ia = 2
  allocate (a%left%left)
  a%left%left%ia = 3
  allocate (a%left%right)
  a%left%right%ia = 4
  allocate (a%right)
  a%right%ia = 5

! Checksum OK?
  if (foo(a) .ne. 15) call abort

! Return pointer to tree item that is present.
  b => bar (a, 3)
  if (.not.associated (b) .or. (b%ia .ne. 3)) call abort
! Return NULL to tree item that is not present.
  b => bar (a, 6)
  if (associated (b)) call abort

! Deallocate to check that there are no memory leaks.
  deallocate (a)
end
