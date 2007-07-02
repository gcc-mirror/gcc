! { dg-do run }
program print_c_kinds
  use, intrinsic :: iso_c_binding
  implicit none

  print *, 'c_short is:              ', c_short
  print *, 'c_int is:                ', c_int
  print *, 'c_long is:               ', c_long
  print *, 'c_long_long is:          ', c_long_long
  print *
  print *, 'c_int8_t is:             ', c_int8_t
  print *, 'c_int_least8_t is:       ', c_int_least8_t
  print *, 'c_int_fast8_t is:        ', c_int_fast8_t
  print *
  print *, 'c_int16_t is:            ', c_int16_t
  print *, 'c_int_least16_t is:      ', c_int_least16_t
  print *, 'c_int_fast16_t is:       ', c_int_fast16_t
  print *
  print *, 'c_int32_t is:            ', c_int32_t
  print *, 'c_int_least32_t is:      ', c_int_least32_t
  print *, 'c_int_fast32_t is:       ', c_int_fast32_t
  print *
  print *, 'c_int64_t is:            ', c_int64_t
  print *, 'c_int_least64_t is:      ', c_int_least64_t
  print *, 'c_int_fast64_t is:       ', c_int_fast64_t
  print *
  print *, 'c_intmax_t is:           ', c_intmax_t
  print *, 'c_intptr_t is:           ', c_intptr_t
  print *
  print *, 'c_float is:              ', c_float
  print *, 'c_double is:             ', c_double
  print *, 'c_long_double is:        ', c_long_double
  print *
  print *, 'c_char is:               ', c_char
end program print_c_kinds
