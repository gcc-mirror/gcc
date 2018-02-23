! { dg-do run }
!
! PR 39630: [F03] Procedure Pointer Components with PASS
!
! taken from "The Fortran 2003 Handbook" (Adams et al., 2009)

module passed_object_example

  type t
    real :: a
    procedure(print_me), pointer, pass(arg) :: proc
  end type t

contains

  subroutine print_me (arg, lun)
    class(t), intent(in) :: arg
    integer, intent(in) :: lun
    if (abs(arg%a-2.718)>1E-6) STOP 1
    write (lun,*) arg%a
  end subroutine print_me

  subroutine print_my_square (arg, lun)
    class(t), intent(in) :: arg
    integer, intent(in) :: lun
    if (abs(arg%a-2.718)>1E-6) STOP 2
    write (lun,*) arg%a**2
  end subroutine print_my_square

end module passed_object_example


program main
  use passed_object_example
  use iso_fortran_env, only: output_unit

  type(t) :: x

  x%a = 2.718
  x%proc => print_me
  call x%proc (output_unit)
  x%proc => print_my_square
  call x%proc (output_unit)

end program main
