/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  union256i_d u;
  int e[8] = { 1, -9, 13, -24, -18, 12, 0, 9 };
  int f[8] = { -24, -18, 12, 0, -9, 13, -24, -18 };

  u.x = _mm256_loadu2_m128i ((__m128i_u *) (e + 1), (__m128i_u *) (e + 3));
  if (check_union256i_d (u, f))
    abort ();
}
