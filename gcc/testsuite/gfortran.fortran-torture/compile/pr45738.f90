PROGRAM TestInfinite
  integer(8) :: bit_pattern_NegInf_i8 = -4503599627370496_8

  integer(8) :: i
  real(8)    :: r

  r = transfer(bit_pattern_NegInf_i8_p,r)
  i = transfer(r,i)

END PROGRAM TestInfinite

