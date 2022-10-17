! PR 101305
! PR 100917
! { dg-do run }
! { dg-additional-sources "typecodes-array-longdouble-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that long double kind constants in the ISO_C_BINDING
! module result in the right type field in arguments passed by descriptor,
! also matching the size of the corresponding C type.  We use 
! assumed-rank arrays to force the use of a descriptor.


program testit
  use iso_c_binding
  implicit none

  interface

    subroutine ctest (arg_long_double, arg_long_double_complex) bind (c)
      use iso_c_binding
      real(C_LONG_DOUBLE) :: arg_long_double(:)
      complex(C_LONG_DOUBLE_COMPLEX) :: arg_long_double_complex(:)
    end subroutine

  end interface

  real(C_LONG_DOUBLE) :: var_long_double(4)
  complex(C_LONG_DOUBLE_COMPLEX) :: var_long_double_complex(4)

  call ctest (var_long_double, var_long_double_complex)

end program
