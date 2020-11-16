/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static int8_t
long_double_to_i8 (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tcfxbr\t} 1 } } */

int
main (void)
{
  assert (long_double_to_i8 (42.L) == 42);
  assert (long_double_to_i8 (-42.L) == -42);
}
