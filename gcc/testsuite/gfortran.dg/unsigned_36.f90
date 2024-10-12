! { dg-do compile }
! { dg-options "-funsigned" }
module use_c_binding
  use iso_c_binding
  implicit none
  unsigned(c_unsigned), bind(c) :: a
  unsigned(c_unsigned_short), bind(c) :: b
  unsigned(c_unsigned_char), bind(c) :: c
  unsigned(c_unsigned_long), bind(c) :: d
  unsigned(c_unsigned_long_long), bind(c) :: e
  unsigned(c_uintmax_t), bind(c) :: f
  unsigned(c_uint8_t), bind(c) :: u8
  unsigned(c_uint16_t), bind(c) :: u16
  unsigned(c_uint32_t), bind(c) :: u32
  unsigned(c_uint64_t), bind(c) :: u64
  unsigned(c_uint_fast8_t), bind(c) :: f8
  unsigned(c_uint_fast16_t), bind(c) :: f16
  unsigned(c_uint_fast32_t), bind(c) :: f32
  unsigned(c_uint_fast64_t), bind(c) :: f64
  unsigned(c_uint_least8_t), bind(c) :: l8
  unsigned(c_uint_least16_t), bind(c) :: l16
  unsigned(c_uint_least32_t), bind(c) :: l32
  unsigned(c_uint_least64_t), bind(c) :: l64
  integer, parameter :: c_128 = c_uint128_t
  integer, parameter :: fast_128 = c_uint_fast128_t
  integer, parameter :: least_128 = c_uint_least128_t
end module use_c_binding

program memain
  use use_c_binding
  use iso_fortran_env
  unsigned(uint8) :: a8
  unsigned(uint16) :: a16
  unsigned(uint32) :: a32
  unsigned(uint64) :: a64
end program memain
