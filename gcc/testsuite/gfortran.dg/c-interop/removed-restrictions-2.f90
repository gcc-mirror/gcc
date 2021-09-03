! { dg-do compile}
!
! TS 29113
! 8.1 Removed restrictions on ISO_C_BINDING module procedures
! 
! The function C_F_PROCPOINTER from the intrinsic module ISO_C_BINDING
! has the restriction in ISO/IEC 1539-1:2010 that CPTR and FPTR shall
! not be the C address and interface of a noninteroperable Fortran
! procedure.
!
! [...]
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

  subroutine test (cptr, fptr)
    type(C_FUNPTR), intent(in) :: cptr
    procedure (foo), pointer, intent(out) :: fptr

    call c_f_procpointer (cptr, fptr)
  end subroutine
end module
