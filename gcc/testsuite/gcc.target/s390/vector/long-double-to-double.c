/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static double
long_double_to_double (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\twflrx\t} 1 } } */

int
main (void)
{
  assert (long_double_to_double (42.L) == 42.);
  assert (long_double_to_double (-42.L) == -42.);
}
