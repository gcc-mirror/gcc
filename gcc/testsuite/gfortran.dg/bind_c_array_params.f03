! { dg-do compile }
module bind_c_array_params
use, intrinsic :: iso_c_binding
implicit none

contains
  subroutine sub0(assumed_array) bind(c) ! { dg-error "cannot be an argument" }
    integer(c_int), dimension(:) :: assumed_array
  end subroutine sub0

  subroutine sub1(deferred_array) bind(c) ! { dg-error "cannot" } 
    integer(c_int), pointer :: deferred_array(:)
  end subroutine sub1
end module bind_c_array_params
