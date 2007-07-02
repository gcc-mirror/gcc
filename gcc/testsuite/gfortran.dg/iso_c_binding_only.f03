! { dg-do compile }
module iso_c_binding_only
  use, intrinsic :: iso_c_binding, only: c_null_ptr
  ! This should be allowed since the C_PTR that the C_NULL_PTR needs will use
  ! a mangled name to prevent collisions.
  integer :: c_ptr
end module iso_c_binding_only
! { dg-final { cleanup-modules "iso_c_binding_only" } }  

