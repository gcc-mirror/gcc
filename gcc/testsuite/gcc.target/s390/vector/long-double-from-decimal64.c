/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
long_double_from_decimal64 (_Decimal64 x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tpfpo\n} 1 } } */

int
main (void)
{
  assert (long_double_from_decimal64 ((_Decimal64) 42) == 42.L);
  assert (long_double_from_decimal64 ((_Decimal64) -42) == -42.L);
}
