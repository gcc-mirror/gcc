! PR 101305
! { dg-do run }
! { dg-additional-sources "establish-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests the CFI_establish function.  All the interesting
! things happen in the corresponding C code.

! Eventually we might want to make the C code pass the descriptors back to
! C-callable Fortran functions, but for now it just checks them internally.

module mm
  use iso_c_binding

  type, bind (c) :: s
    integer(C_INT) :: i, j
  end type
end module
  

program testit
  use iso_c_binding
  use mm
  implicit none

  interface

    subroutine ctest_establish () bind (c)
    end subroutine

  end interface

  call ctest_establish ()

end program
