! { dg-do compile }
! PR42008 Wrongly rejected derived types with default initializers
! in PURE procedures
module mod_xyz
 implicit none
contains
 pure subroutine psub()
  type ilist
    type(ilist), pointer :: next => null() ! Valid
    integer :: i
  end type ilist
 end subroutine psub
end module mod_xyz

module mod_xyz2
 implicit none
contains
 pure subroutine psub()
  type ilist
    type(ilist), pointer :: next
    integer, pointer :: p => null() ! Valid
    integer :: i
  end type ilist
  type(ilist) :: var ! Valid
  var%next => null()
 end subroutine psub
end module mod_xyz2

module mod_xyz3
 implicit none
 type ilist
   type(ilist), pointer :: next => null() ! Valid
   integer :: i
 end type ilist
contains
 pure subroutine psub()
  type(ilist) :: var ! Valid
 end subroutine psub
end module mod_xyz3

pure function test()
  integer,pointer :: p => null() !{ dg-error "not allowed in a PURE procedure" }
  integer :: test
  test = p
end function test
