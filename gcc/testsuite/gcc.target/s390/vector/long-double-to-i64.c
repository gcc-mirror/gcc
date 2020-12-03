/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static int64_t
long_double_to_i64 (long double x)
{
  return x;
}

/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,1\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,5\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tcgxbr\t} 1 } } */

int
main (void)
{
  assert (long_double_to_i64 (42.L) == 42);
  assert (long_double_to_i64 (-42.L) == -42);
}
