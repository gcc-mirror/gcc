! PR92482
! { dg-do run }
! { dg-additional-sources "fc-descriptor-5-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests it works to call a C function from Fortran with 
! an assumed length character dummy.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      character(len=*,kind=C_CHAR) :: a
    end subroutine
  end interface

  character(len=20,kind=C_CHAR) :: aa

  ! Test both passing the fixed-length string directly to the function
  ! with a C interface, and indirectly via a Fortran function with an
  ! assumed-length dummy argument.
  call ctest (aa)
  call ftest (aa)

contains
  subroutine ftest (a)
    use iso_c_binding
    character(len=*,kind=C_CHAR) :: a
    call ctest (a)
  end subroutine

end program
