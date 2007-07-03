! { dg-do compile }
module iso_c_binding_only
  ! c_f_procpointer verifies that the c_funptr derived type for the cptr param
  ! is auto-generated, and c_f_pointer tests c_ptr.
  use, intrinsic :: iso_c_binding, only: c_null_ptr, c_f_procpointer
  ! This should be allowed since the C_PTR that the C_NULL_PTR needs will use
  ! a mangled name to prevent collisions.
  integer :: c_ptr
end module iso_c_binding_only
! { dg-final { cleanup-modules "iso_c_binding_only" } }  

