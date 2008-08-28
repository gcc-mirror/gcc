/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

__m256i
__attribute__((noinline))
foo (int x1, int x2, int x3, int x4,
     int x5, int x6, int x7, int x8)
{
  return _mm256_set_epi32 (x1, x2, x3, x4, x5, x6, x7, x8);
}

static void
avx_test (void)
{
  int v[8] = { -3, 2, 1, 9, 23, -173, -13, 69 };
  union256i_d u;

  u.x = foo (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union256i_d (u, v))
     abort ();
}
