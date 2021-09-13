! PR 101310
! { dg-do run }
! { dg-additional-sources "section-4-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests various scenarios with using CFI_section to extract
! a section with rank less than the source array.  Everything interesting
! happens on the C side.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest () bind (c)
      use iso_c_binding
    end subroutine

  end interface

  call ctest ()

end program
