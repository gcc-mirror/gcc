! { dg-do run }
!
! PR39630: Fortran 2003: Procedure pointer components.
!
! test case taken from:
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/4a827e8ced6efb0f/884b9eca6d7e6742?#884b9eca6d7e6742
! http://fortranwiki.org/fortran/show/proc_component_example

module proc_component_example

  type t
    real :: a
    procedure(print_int), pointer, &
                          nopass :: proc
  end type t

  abstract interface
    subroutine print_int (arg, lun)
      import
      type(t), intent(in) :: arg
      integer, intent(in) :: lun
    end subroutine print_int
  end interface

  integer :: calls = 0

contains

  subroutine print_me (arg, lun)
    type(t), intent(in) :: arg
    integer, intent(in) :: lun
    write (lun,*) arg%a
    calls = calls + 1
  end subroutine print_me

  subroutine print_my_square (arg, lun)
    type(t), intent(in) :: arg
    integer, intent(in) :: lun
    write (lun,*) arg%a**2
    calls = calls + 1
  end subroutine print_my_square

end module proc_component_example

program main

    use proc_component_example
    use iso_fortran_env, only : output_unit

    type(t) :: x

    x%a = 2.71828

    x%proc => print_me
    call x%proc(x, output_unit)
    x%proc => print_my_square
    call x%proc(x, output_unit)

    if (calls/=2) call abort

end program main 

! { dg-final { cleanup-modules "proc_component_example" } }

