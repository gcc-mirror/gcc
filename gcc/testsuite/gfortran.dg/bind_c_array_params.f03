! { dg-do compile }
! { dg-options "-std=f2003" }
module bind_c_array_params
use, intrinsic :: iso_c_binding
implicit none

contains
  subroutine sub0(assumed_array) bind(c) ! { dg-error "TS 29113/TS 18508: Assumed-shape array 'assumed_array' at .1. as dummy argument to the BIND.C. procedure 'sub0'" }
    integer(c_int), dimension(:) :: assumed_array
  end subroutine sub0

  subroutine sub1(deferred_array) bind(c) ! { dg-error "TS 29113/TS 18508: Variable 'deferred_array' at .1. with POINTER attribute in procedure 'sub1' with BIND.C." }
    integer(c_int), pointer :: deferred_array(:)
  end subroutine sub1
end module bind_c_array_params
