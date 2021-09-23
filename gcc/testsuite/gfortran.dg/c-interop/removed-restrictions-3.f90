! { dg-do compile}
!
! TS 29113
! 8.1 Removed restrictions on ISO_C_BINDING module procedures
! 
! The function C_LOC from the intrinsic module ISO_C_BINDING has the
! restriction in ISO/IEC 1539-1:2010 that if X is an array, it shall 
! be of interoperable type.
!
! [...]
!
! These restrictions are removed.

module m
  use ISO_C_BINDING
  implicit none

  ! An obvious example of a type that isn't interoperable is a
  ! derived type without a bind(c) clause.

  integer :: buflen
  parameter (buflen=256)

  type :: packet
    integer :: size
    integer(1) :: buf(buflen)
  end type

contains

  subroutine test (packets, ptr)
    type(packet), pointer, intent(in) :: packets(:)
    type(C_PTR), intent(out) :: ptr

    ptr = c_loc (packets)
  end subroutine
end module
