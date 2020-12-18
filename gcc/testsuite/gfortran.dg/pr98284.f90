! { dg-do compile }
! PR fortran/98284 - ICE in get_array_index

program p
  implicit none
  type t
     integer, allocatable :: h(:)
  end type t
  type(t) :: u
  integer :: i
  data (u% h(i),i=1,8) /8*1/ ! { dg-error "cannot have the ALLOCATABLE attribute" }
end
