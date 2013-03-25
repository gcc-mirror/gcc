! { dg-do compile }
!
! PR fortran/54263
!
use iso_c_binding
type(c_ptr) :: cp
integer, pointer :: p
call c_f_pointer (cp, p, shape=[2]) ! { dg-error "Unexpected SHAPE argument at .1. to C_F_POINTER with scalar FPTR" }
end
