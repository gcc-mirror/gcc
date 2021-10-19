! PR 101305
! PR 100917
! xfailed due to PR 101308
! { dg-do run }
! { dg-additional-sources "typecodes-scalar-basic-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that kind constants in the ISO_C_BINDING
! module result in the right type field in arguments passed by descriptor,
! also matching the size of the corresponding C type.  We use pointers
! to force the use of a descriptor.
!
! Some types are tested in their own testcases to allow conditionalization
! for target-specific support or xfailing to track bugs.

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

    subroutine ctest_int1 (arg_int, arg_short, arg_long, arg_long_long, &
                           arg_signed_char) bind (c)
      use iso_c_binding
      integer(C_INT), pointer :: arg_int
      integer(C_SHORT), pointer :: arg_short
      integer(C_LONG), pointer :: arg_long
      integer(C_LONG_LONG), pointer :: arg_long_long
      integer(C_SIGNED_CHAR), pointer :: arg_signed_char
    end subroutine

    subroutine ctest_int2 (arg_int8, arg_int16, arg_int32, arg_int64) bind (c)
      use iso_c_binding
      integer(C_INT8_T), pointer :: arg_int8
      integer(C_INT16_T), pointer :: arg_int16
      integer(C_INT32_T), pointer :: arg_int32
      integer(C_INT64_T), pointer :: arg_int64
    end subroutine

    subroutine ctest_int3 (arg_least8, arg_least16, arg_least32, &
                           arg_least64) bind (c)
      use iso_c_binding
      integer(C_INT_LEAST8_T), pointer :: arg_least8
      integer(C_INT_LEAST16_T), pointer :: arg_least16
      integer(C_INT_LEAST32_T), pointer :: arg_least32
      integer(C_INT_LEAST64_T), pointer :: arg_least64
    end subroutine

    subroutine ctest_int4 (arg_fast8, arg_fast16, arg_fast32, &
                           arg_fast64) bind (c)
      use iso_c_binding
      integer(C_INT_FAST8_T), pointer :: arg_fast8
      integer(C_INT_FAST16_T), pointer :: arg_fast16
      integer(C_INT_FAST32_T), pointer :: arg_fast32
      integer(C_INT_FAST64_T), pointer :: arg_fast64
    end subroutine

    subroutine ctest_int5 (arg_size, arg_intmax, arg_intptr, &
                           arg_ptrdiff) bind (c)
      use iso_c_binding
      integer(C_SIZE_T), pointer :: arg_size
      integer(C_INTMAX_T), pointer :: arg_intmax
      integer(C_INTPTR_T), pointer :: arg_intptr
      integer(C_PTRDIFF_T), pointer :: arg_ptrdiff
    end subroutine

    subroutine ctest_real (arg_float, arg_double) bind (c)
      use iso_c_binding
      real(C_FLOAT), pointer :: arg_float
      real(C_DOUBLE), pointer :: arg_double
    end subroutine

    subroutine ctest_complex (arg_float_complex, arg_double_complex) &
                             bind (c)
      use iso_c_binding
      complex(C_FLOAT_COMPLEX), pointer :: arg_float_complex
      complex(C_DOUBLE_COMPLEX), pointer :: arg_double_complex
    end subroutine

    subroutine ctest_misc (arg_bool, arg_cptr, arg_cfunptr, arg_struct) &
                          bind (c)
      use iso_c_binding
      use mm
      logical(C_BOOL), pointer :: arg_bool
      type(C_PTR), pointer :: arg_cptr
      type(C_FUNPTR), pointer :: arg_cfunptr
      type(s), pointer :: arg_struct
    end subroutine

  end interface

  integer(C_INT), pointer :: var_int
  integer(C_SHORT), pointer :: var_short
  integer(C_LONG), pointer :: var_long
  integer(C_LONG_LONG), pointer :: var_long_long
  integer(C_SIGNED_CHAR), pointer :: var_signed_char
  integer(C_INT8_T), pointer :: var_int8
  integer(C_INT16_T), pointer :: var_int16
  integer(C_INT32_T), pointer :: var_int32
  integer(C_INT64_T), pointer :: var_int64
  integer(C_INT_LEAST8_T), pointer :: var_least8
  integer(C_INT_LEAST16_T), pointer :: var_least16
  integer(C_INT_LEAST32_T), pointer :: var_least32
  integer(C_INT_LEAST64_T), pointer :: var_least64
  integer(C_INT_FAST8_T), pointer :: var_fast8
  integer(C_INT_FAST16_T), pointer :: var_fast16
  integer(C_INT_FAST32_T), pointer :: var_fast32
  integer(C_INT_FAST64_T), pointer :: var_fast64
  integer(C_SIZE_T), pointer :: var_size
  integer(C_INTMAX_T), pointer :: var_intmax
  integer(C_INTPTR_T), pointer :: var_intptr
  integer(C_PTRDIFF_T), pointer :: var_ptrdiff
  real(C_FLOAT), pointer :: var_float
  real(C_DOUBLE), pointer :: var_double
  complex(C_FLOAT_COMPLEX), pointer :: var_float_complex
  complex(C_DOUBLE_COMPLEX), pointer :: var_double_complex
  logical(C_BOOL), pointer :: var_bool
  type(C_PTR), pointer :: var_cptr
  type(C_FUNPTR), pointer :: var_cfunptr
  type(s), pointer :: var_struct

  nullify (var_int, var_short, var_long, var_long_long, var_signed_char)
  call ctest_int1 (var_int, var_short, var_long, var_long_long, &
                   var_signed_char)

  nullify (var_int8, var_int16, var_int32, var_int64)
  call ctest_int2 (var_int8, var_int16, var_int32, var_int64)

  nullify (var_least8, var_least16, var_least32, var_least64)
  call ctest_int3 (var_least8, var_least16, var_least32, var_least64)

  nullify (var_fast8, var_fast16, var_fast32, var_fast64)
  call ctest_int4 (var_fast8, var_fast16, var_fast32, var_fast64)

  nullify (var_size, var_intmax, var_intptr, var_ptrdiff)
  call ctest_int5 (var_size, var_intmax, var_intptr, var_ptrdiff)

  nullify (var_float, var_double)
  call ctest_real (var_float, var_double)

  nullify (var_float_complex, var_double_complex)
  call ctest_complex (var_float_complex, var_double_complex)

  nullify (var_bool, var_cptr, var_cfunptr, var_struct)
  call ctest_misc (var_bool, var_cptr, var_cfunptr, var_struct)

  ! FIXME: how do you pass something that corresponds to CFI_type_other?
  ! The Fortran front end complains if you try to pass something that
  ! isn't interoperable, such as a derived type object without bind(c).

end program
