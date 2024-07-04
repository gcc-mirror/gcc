/* PR target/112518 */
/* { dg-do run { target { bmi2 && int128 } } } */
/* { dg-options "-Os -mbmi2" } */

#include "bmi2-check.h"

unsigned u;
int g;

unsigned long long
foo (int i)
{
  unsigned long long x = u;
  g = __builtin_mul_overflow_p (u, ((unsigned __int128) 4292468825) << 64 | 150, 0);
  x |= g % i;
  return x;
}

static __attribute__((noipa)) void
bmi2_test ()
{
  unsigned long long x = foo (3);
  if (x)
    __builtin_abort ();
}
