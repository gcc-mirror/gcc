! { dg-do run }
! { dg-additional-sources "allocate-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests the CFI_allocate and CFI_deallocate functions.  
! All the interesting things happen in the corresponding C code.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest () bind (c)
    end subroutine
  end interface

  call ctest ()

end program
