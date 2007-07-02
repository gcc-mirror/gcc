! { dg-do compile }
module bind_c_dts_3
use, intrinsic :: iso_c_binding
implicit none

TYPE, bind(c) :: t
  integer(c_int) :: i
end type t

type :: my_c_type_0 ! { dg-error "must have the BIND attribute" }
   integer(c_int) :: i
end type my_c_type_0

type, bind(c) :: my_c_type_1 ! { dg-error "BIND.C. derived type" }
   type(my_c_type_0) :: my_nested_type
   type(c_ptr) :: c_address
   integer(c_int), pointer :: j ! { dg-error "cannot have the POINTER" }
end type my_c_type_1

type, bind(c) :: t2 ! { dg-error "BIND.C. derived type" }
   type (t2), pointer :: next ! { dg-error "cannot have the POINTER" }
end type t2

type, bind(c):: t3 ! { dg-error "BIND.C. derived type" }
  type(t), allocatable :: c(:) ! { dg-error "cannot have the ALLOCATABLE" }
end type t3

contains
  subroutine sub0(my_type, expected_value) bind(c) ! { dg-error "is not C interoperable" }
    type(my_c_type_1) :: my_type
    integer(c_int), value :: expected_value

    if (my_type%my_nested_type%i .ne. expected_value) then
       call abort ()
    end if
  end subroutine sub0
end module bind_c_dts_3

! { dg-final { cleanup-modules "bind_c_dts_3" } }
