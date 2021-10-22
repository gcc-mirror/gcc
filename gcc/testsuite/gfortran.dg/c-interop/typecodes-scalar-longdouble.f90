! xfailed due to PR 101308
! PR 101305
! PR 100917
! { dg-do run }
! { dg-additional-sources "typecodes-scalar-longdouble-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that long double kind constants in the ISO_C_BINDING
! module result in the right type field in arguments passed by descriptor,
! also matching the size of the corresponding C type.  We use pointers
! to force the use of a descriptor.

program testit
  use iso_c_binding
  implicit none

  interface

    subroutine ctest (arg_long_double, arg_long_double_complex) bind (c)
      use iso_c_binding
      real(C_LONG_DOUBLE), pointer :: arg_long_double
      complex(C_LONG_DOUBLE_COMPLEX), pointer :: arg_long_double_complex
    end subroutine

  end interface

  real(C_LONG_DOUBLE), pointer :: var_long_double
  complex(C_LONG_DOUBLE_COMPLEX), pointer :: var_long_double_complex

  nullify (var_long_double, var_long_double_complex)
  call ctest (var_long_double, var_long_double_complex)

end program
