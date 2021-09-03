! { dg-do run }
! { dg-additional-sources "setpointer-c.c dump-descriptors.c" }
!
! This program tests the CFI_setpointer function.  All the interesting
! things happen in the corresponding C code.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest () bind (c)
    end subroutine
  end interface

  call ctest ()

end program
