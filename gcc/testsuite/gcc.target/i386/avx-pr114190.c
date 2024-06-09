/* PR rtl-optimization/114190 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -fno-dce -fharden-compares -mavx --param=max-rtl-if-conversion-unpredictable-cost=136 -mno-avx512f -Wno-psabi" } */

#include "avx-check.h"

typedef unsigned char U __attribute__((vector_size (64)));
typedef unsigned int V __attribute__((vector_size (64)));
U u;

V
foo (V a, V b)
{
  u[0] = __builtin_sub_overflow (0, (int) a[0], &a[b[7] & 5]) ? -u[1] : -b[3];
  b ^= 0 != b;
  return (V) u + (V) a + (V) b;
}

static void
avx_test (void)
{
  V x = foo ((V) { 1 }, (V) { 0, 0, 0, 1 });
  if (x[0] != -1U)
    __builtin_abort ();
  if (x[3] != -2U)
    __builtin_abort ();
}
