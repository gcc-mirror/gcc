! PR 101305
! PR 92482
! { dg-do run }
! { dg-additional-sources "typecodes-array-char-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that the character kind constants provided by
! gfortran's ISO_C_BINDING module result in the right type field in
! arguments passed by descriptor, also matching the size of the corresponding
! C type.  We use assumed-shape arrays to force the use of a descriptor.
!
! FIXME: because of PR92482, we can only test len=1 characters.  This
! test should be extended once that bug is fixed.

program testit
  use iso_c_binding
  implicit none

  integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

  interface

    subroutine ctest_1 (arg_cchar, arg_ucs4) bind (c)
      use iso_c_binding
      integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')
      character(kind=C_CHAR) :: arg_cchar(:)
      character(kind=ucs4) :: arg_ucs4(:)
    end subroutine

    subroutine ctest_5 (arg_cchar, arg_ucs4) bind (c)
      use iso_c_binding
      integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')
      character(kind=C_CHAR,len=*) :: arg_cchar(:)
      character(kind=ucs4,len=*) :: arg_ucs4(:)
    end subroutine

  end interface

  character(kind=C_CHAR) :: var_cchar(4)
  character(kind=ucs4) :: var_ucs4(4)
  character(kind=C_CHAR,len=5) :: var_cchar_5(4)
  character(kind=ucs4,len=5) :: var_ucs4_5(4)

  call ctest_1 (var_cchar, var_ucs4)
  call ctest_5 (var_cchar_5, var_ucs4_5)

end program
