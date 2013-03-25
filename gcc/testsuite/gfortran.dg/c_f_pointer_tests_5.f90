! { dg-do compile }
!
! PR 54667: [OOP] gimplification failure with c_f_pointer
!
! Contributed by Andrew Benson <abensonca@gmail.com>

use, intrinsic :: ISO_C_Binding
type :: nc
end type
type(c_ptr) :: cSelf
class(nc), pointer :: self
call c_f_pointer(cSelf, self)  ! { dg-error "shall not be polymorphic" }
end
