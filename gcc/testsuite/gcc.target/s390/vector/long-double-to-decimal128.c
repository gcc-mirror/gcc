/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static _Decimal128
long_double_to_decimal128 (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tpfpo\n} 1 } } */

int
main (void)
{
  assert (long_double_to_decimal128 (42.L) == (_Decimal128) 42.);
  assert (long_double_to_decimal128 (-42.L) == (_Decimal128) -42.);
}
