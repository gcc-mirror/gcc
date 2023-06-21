! { dg-do compile }
!
! TS 29113
! 8.1 Removed restrictions on ISO_C_BINDING module procedures
!
! The subroutine C_F_POINTER from the intrinsic module ISO_C_BINDING has
! the restriction in ISO/IEC 1539- 1:2010 that if FPTR is an array, it
! shall be of interoperable type.
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

  subroutine test (ptr, n, packets)
    type(C_PTR), intent(in) :: ptr
    integer, intent(in) :: n
    type(packet), pointer, intent(out) :: packets(:)

    integer :: s(1)
    s(1) = n

    call c_f_pointer (ptr, packets, s)
  end subroutine
end module
