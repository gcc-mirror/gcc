/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static float
long_double_to_float (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\twflrx\t%v\d+,%v\d+,0,3\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tledbr\t} 1 } } */

int
main (void)
{
  assert (long_double_to_float (42.L) == 42.F);
  assert (long_double_to_float (-42.L) == -42.F);
}
