! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/32601
module pr32601
use, intrinsic :: iso_c_binding, only: c_int
contains
  function get_ptr()
    integer(c_int), pointer :: get_ptr
    integer(c_int), target :: x
    get_ptr = x
  end function get_ptr
end module pr32601

USE ISO_C_BINDING, only: c_null_ptr, c_ptr, c_loc
use pr32601
implicit none

type(c_ptr) :: t
t = c_null_ptr

! Next two lines should be errors if -pedantic or -std=f2003
print *, c_null_ptr, t  ! { dg-error "has PRIVATE components" }
print *, t ! { dg-error "has PRIVATE components" }

print *, c_loc(get_ptr()) ! { dg-error "has PRIVATE components" }

end
! { dg-final { cleanup-modules "pr32601" } }  
