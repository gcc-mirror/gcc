/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
long_double_copysign (long double x, long double y)
{
  return __builtin_copysignl (x, y);
}

/* { dg-final { scan-assembler-times {\n\tvsel\t} 1 } } */

int
main (void)
{
  assert (long_double_copysign (1.1L, 2.2L) == 1.1L);
  assert (long_double_copysign (1.1L, -2.2L) == -1.1L);
  assert (long_double_copysign (-1.1L, 2.2L) == 1.1L);
  assert (long_double_copysign (-1.1L, -2.2L) == -1.1L);
}
