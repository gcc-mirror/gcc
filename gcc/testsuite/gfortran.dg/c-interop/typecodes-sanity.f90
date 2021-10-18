! PR 101305
! { dg-do run }
! { dg-additional-sources "typecodes-sanity-c.c" }
! { dg-additional-options "-g" }
!
! This program does sanity checking on the CFI_type_* macros.  All
! of the interesting things happen on the C side.

program testit
  use iso_c_binding
  implicit none

  integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

  interface

    subroutine ctest_typecodes () bind (c)
    end subroutine

  end interface

  call ctest_typecodes ()

end program
