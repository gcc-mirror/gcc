! { dg-do "compile" }
! PR fortran/42354

use iso_c_binding
implicit none
integer, target :: a
type t
  type(c_ptr) :: ptr = c_loc(a)    ! { dg-error "must be an intrinsic function" }
end type t
type(c_ptr) :: ptr2 = c_loc(a)     ! { dg-error "must be an intrinsic function" }
end
