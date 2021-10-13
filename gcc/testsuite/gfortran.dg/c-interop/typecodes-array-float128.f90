! PR 101305
! PR 100914
! PR 100917
! Fails on x86 targets where sizeof(long double) == 16 (PR100917).
! { dg-do run }
! { dg-require-effective-target fortran_real_c_float128 }
! { dg-additional-sources "typecodes-array-float128-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that the vendor extension kind constants provided by
! gfortran's ISO_C_BINDING module result in the right type field in
! arguments passed by descriptor, also matching the size of the corresponding
! C type.  We use assumed-shape arrays to force the use of a descriptor.

program testit
  use iso_c_binding
  implicit none

  interface

    subroutine ctest (arg_float128, arg_complex128) bind (c)
      use iso_c_binding
      real(C_FLOAT128) :: arg_float128(:)
      complex(C_FLOAT128_COMPLEX) :: arg_complex128(:)
    end subroutine

  end interface

  real(C_FLOAT128) :: var_float128(4)
  complex(C_FLOAT128_COMPLEX) :: var_complex128(4)

  call ctest (var_float128, var_complex128)

end program
