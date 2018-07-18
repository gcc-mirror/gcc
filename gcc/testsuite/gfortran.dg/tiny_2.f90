! { dg-do run }
program tiny2
  real(4) x4
  real(8) x8
  x4 = tiny(x4)
  x8 = tiny(x8)
  if (minexponent(x4) /= exponent(x4)) STOP 1
  if (minexponent(x8) /= exponent(x8)) STOP 2
end program tiny2
