! { dg-do compile }
! { dg-options "" }
!
! PR fortran/32601
use, intrinsic :: iso_c_binding, only: c_loc, c_ptr
implicit none

! This was causing an ICE, but is an error because the argument to C_LOC 
! needs to be a variable.
print *, c_loc(4) ! { dg-error "shall have either the POINTER or the TARGET attribute" }

end
