! PR92482
! { dg-do run }
! { dg-additional-sources "fc-out-descriptor-5-c.c dump-descriptors.c" }
!
! This program checks that you can call a C function declared with an
! assumed-length character dummy from Fortran.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      character(len=*,kind=C_CHAR), intent(out) :: a
    end subroutine
  end interface

  character(len=26,kind=C_CHAR) :: aa
  aa = 'abcdefghijklmnopqrstuvwxyz'

  ! Test both passing the fixed-length-string directly to the function
  ! with a C interface, and indirectly via a Fortran function with an
  ! assumed-length dummy argument.
  call ctest (aa)
  call ftest (aa)

contains
  subroutine ftest (a) bind (c)
    use iso_c_binding
    character(len=*,kind=C_CHAR), intent(out) :: a
    call ctest (a)
  end subroutine

end program
