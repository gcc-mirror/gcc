! { dg-do compile }
!
! TS 29113
! 8.1 Removed restrictions on ISO_C_BINDING module procedures
!
! [...]
!
! The function C_FUNLOC from the intrinsic module ISO_C_BINDING has
! the restriction in ISO/IEC 1539-1:2010 that its argument shall be
! interoperable.
!
! These restrictions are removed.

module m
  use ISO_C_BINDING
  implicit none

  ! Declare a non-interoperable Fortran procedure interface.
  abstract interface
    function foo (x, y)
      integer :: foo
      integer, intent (in) :: x, y
    end function
  end interface

contains

  subroutine test (fptr, cptr)
    procedure (foo), pointer, intent(in) :: fptr
    type(C_FUNPTR), intent(out) :: cptr

    cptr = c_funloc (fptr)
  end subroutine
end module
