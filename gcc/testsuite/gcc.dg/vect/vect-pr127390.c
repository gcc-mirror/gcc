/* { dg-additional-options "-O3" } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target bitint } */

#include <stdint.h>
#include "tree-vect.h"

typedef signed _BitInt(24) i24;
int8_t m0[2];

int __attribute__((noipa))
foo (int16_t in2)
{
  int16_t x2;
  int n = 0;
  for (n = 0; n < 8; n++) {
    if (((i24)(m0[1] ? m0[1] * 2 : 1) >= (i24)(x2 - 1))) break;
  }
  return n;
}

int main(void)
{
  check_vect ();
  if (foo (-1))
    __builtin_abort ();
  return 0;
}
