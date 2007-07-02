! { dg-do compile }
use, intrinsic :: iso_c_binding
type, bind(c) :: mytype
  integer(c_int) :: j
end type mytype

type(mytype), bind(c) :: mytype_var ! { dg-error "cannot be BIND.C." }

integer(c_int), bind(c) :: i ! { dg-error "cannot be declared with BIND.C." }
integer(c_int), bind(c), dimension(10) :: my_array ! { dg-error "cannot be BIND.C." }

common /COM/ i
bind(c) :: /com/

end
