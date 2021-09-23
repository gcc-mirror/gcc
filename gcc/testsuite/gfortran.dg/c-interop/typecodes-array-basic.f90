! PR 101305
! PR 100917
! { dg-do run }
! { dg-additional-sources "typecodes-array-basic-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests that kind constants in the ISO_C_BINDING
! module result in the right type field in arguments passed by descriptor,
! also matching the size of the corresponding C type.  We use 
! assumed-rank arrays to force the use of a descriptor.
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
      integer(C_INT) :: arg_int(:)
      integer(C_SHORT) :: arg_short(:)
      integer(C_LONG) :: arg_long(:)
      integer(C_LONG_LONG) :: arg_long_long(:)
      integer(C_SIGNED_CHAR) :: arg_signed_char(:)
    end subroutine

    subroutine ctest_int2 (arg_int8, arg_int16, arg_int32, arg_int64) bind (c)
      use iso_c_binding
      integer(C_INT8_T) :: arg_int8(:)
      integer(C_INT16_T) :: arg_int16(:)
      integer(C_INT32_T) :: arg_int32(:)
      integer(C_INT64_T) :: arg_int64(:)
    end subroutine

    subroutine ctest_int3 (arg_least8, arg_least16, arg_least32, &
                           arg_least64) bind (c)
      use iso_c_binding
      integer(C_INT_LEAST8_T) :: arg_least8(:)
      integer(C_INT_LEAST16_T) :: arg_least16(:)
      integer(C_INT_LEAST32_T) :: arg_least32(:)
      integer(C_INT_LEAST64_T) :: arg_least64(:)
    end subroutine

    subroutine ctest_int4 (arg_fast8, arg_fast16, arg_fast32, &
                           arg_fast64) bind (c)
      use iso_c_binding
      integer(C_INT_FAST8_T) :: arg_fast8(:)
      integer(C_INT_FAST16_T) :: arg_fast16(:)
      integer(C_INT_FAST32_T) :: arg_fast32(:)
      integer(C_INT_FAST64_T) :: arg_fast64(:)
    end subroutine

    subroutine ctest_int5 (arg_size, arg_intmax, arg_intptr, &
                           arg_ptrdiff) bind (c)
      use iso_c_binding
      integer(C_SIZE_T) :: arg_size(:)
      integer(C_INTMAX_T) :: arg_intmax(:)
      integer(C_INTPTR_T) :: arg_intptr(:)
      integer(C_PTRDIFF_T) :: arg_ptrdiff(:)
    end subroutine

    subroutine ctest_real (arg_float, arg_double) bind (c)
      use iso_c_binding
      real(C_FLOAT) :: arg_float(:)
      real(C_DOUBLE) :: arg_double(:)
    end subroutine

    subroutine ctest_complex (arg_float_complex, arg_double_complex) &
                             bind (c)
      use iso_c_binding
      complex(C_FLOAT_COMPLEX) :: arg_float_complex(:)
      complex(C_DOUBLE_COMPLEX) :: arg_double_complex(:)
    end subroutine

    subroutine ctest_misc (arg_bool, arg_cptr, arg_cfunptr, &
                           arg_struct) bind (c)
      use iso_c_binding
      use mm
      logical(C_BOOL) :: arg_bool(:)
      type(C_PTR) :: arg_cptr(:)
      type(C_FUNPTR) :: arg_cfunptr(:)
      type(s) :: arg_struct(:)
    end subroutine

  end interface

  integer(C_INT) :: var_int(4)
  integer(C_SHORT) :: var_short(4)
  integer(C_LONG) :: var_long(4)
  integer(C_LONG_LONG) :: var_long_long(4)
  integer(C_SIGNED_CHAR) :: var_signed_char(4)
  integer(C_INT8_T) :: var_int8(4)
  integer(C_INT16_T) :: var_int16(4)
  integer(C_INT32_T) :: var_int32(4)
  integer(C_INT64_T) :: var_int64(4)
  integer(C_INT_LEAST8_T) :: var_least8(4)
  integer(C_INT_LEAST16_T) :: var_least16(4)
  integer(C_INT_LEAST32_T) :: var_least32(4)
  integer(C_INT_LEAST64_T) :: var_least64(4)
  integer(C_INT_FAST8_T) :: var_fast8(4)
  integer(C_INT_FAST16_T) :: var_fast16(4)
  integer(C_INT_FAST32_T) :: var_fast32(4)
  integer(C_INT_FAST64_T) :: var_fast64(4)
  integer(C_SIZE_T) :: var_size(4)
  integer(C_INTMAX_T) :: var_intmax(4)
  integer(C_INTPTR_T) :: var_intptr(4)
  integer(C_PTRDIFF_T) :: var_ptrdiff(4)
  real(C_FLOAT) :: var_float(4)
  real(C_DOUBLE) :: var_double(4)
  complex(C_FLOAT_COMPLEX) :: var_float_complex(4)
  complex(C_DOUBLE_COMPLEX) :: var_double_complex(4)
  logical(C_BOOL) :: var_bool(4)
  type(C_PTR) :: var_cptr(4)
  type(C_FUNPTR) :: var_cfunptr(4)
  type(s) :: var_struct(4)

  call ctest_int1 (var_int, var_short, var_long, var_long_long, &
                   var_signed_char)

  call ctest_int2 (var_int8, var_int16, var_int32, var_int64)

  call ctest_int3 (var_least8, var_least16, var_least32, var_least64)

  call ctest_int4 (var_fast8, var_fast16, var_fast32, var_fast64)

  call ctest_int5 (var_size, var_intmax, var_intptr, var_ptrdiff)

  call ctest_real (var_float, var_double)

  call ctest_complex (var_float_complex, var_double_complex)

  call ctest_misc (var_bool, var_cptr, var_cfunptr, var_struct)

  ! FIXME: how do you pass something that corresponds to CFI_type_other?
  ! The Fortran front end complains if you try to pass something that
  ! isn't interoperable, such as a derived type object without bind(c).

end program
