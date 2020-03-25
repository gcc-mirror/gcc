/* PR tree-optimization/94300 */
/* { dg-do run { target { avx512f } } } */
/* { dg-options "-O1 -mavx512f -mprefer-vector-width=512 -mtune=skylake-avx512" } */

#include "avx512f-check.h"

typedef double V __attribute__((vector_size (64)));

static void
avx512f_test (void)
{
  double mem[16];
  const V a = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
  const V b = { 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0 };
  V c;
  __builtin_memcpy (mem, &a, 64);
  __builtin_memcpy (mem + 8, &b, 64);
  __builtin_memcpy (&c, mem + 4, 64);
  if (c[5] != 9.0)
    __builtin_abort ();
}
