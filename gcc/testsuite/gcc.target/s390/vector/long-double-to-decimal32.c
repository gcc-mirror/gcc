/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static _Decimal32
long_double_to_decimal32 (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tpfpo\n} 1 } } */

int
main (void)
{
  assert (long_double_to_decimal32 (42.L) == (_Decimal32) 42.);
  assert (long_double_to_decimal32 (-42.L) == (_Decimal32) -42.);
}
