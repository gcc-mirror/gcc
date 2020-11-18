/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
long_double_from_i32 (int32_t x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tcxfbr\t} 1 } } */

int
main (void)
{
  assert (long_double_from_i32 (42) == 42.L);
  assert (long_double_from_i32 (-42) == -42.L);
}
