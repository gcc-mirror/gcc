! { dg-do compile }
!
! PR 60302: [4.9 Regression] ICE with c_f_pointer and android cross compiler
!
! Contributed by Valery Weber <valeryweber@hotmail.com>

subroutine reshape_inplace_c2_c2 (new_shape)
  use, intrinsic :: iso_c_binding
  implicit none
  integer :: new_shape(:)
  complex, pointer :: ptr_x(:)
  type(c_ptr) :: loc_x
  call c_f_pointer (loc_x, ptr_x, new_shape)
end subroutine
