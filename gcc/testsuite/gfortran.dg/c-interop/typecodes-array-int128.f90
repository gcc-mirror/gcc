! PR 101305
! { dg-do run }
! { dg-require-effective-target fortran_integer_16 }
! { dg-additional-sources "typecodes-array-int128-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that 128-bit integer kind constants provided by
! gfortran's ISO_C_BINDING module result in the right type field in
! arguments passed by descriptor, also matching the size of the corresponding
! C type.  We use assumed-shape arrays to force the use of a descriptor.

program testit
  use iso_c_binding
  implicit none

  interface

    subroutine ctest (arg_int128, arg_least128, arg_fast128) bind (c)
      use iso_c_binding
      integer(C_INT128_T) :: arg_int128(:)
      integer(C_INT_LEAST128_T) :: arg_least128(:)
      integer(C_INT_FAST128_T) :: arg_fast128(:)
    end subroutine

  end interface

  integer(C_INT128_T) :: var_int128(4)
  integer(C_INT_LEAST128_T) :: var_least128(4)
  integer(C_INT_FAST128_T) :: var_fast128(4)

  call ctest (var_int128, var_least128, var_fast128)

end program
