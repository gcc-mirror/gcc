/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  union256d u;
  double e[8] = { 1.5, -9.5, 13.25, -24.75, -18.75, 12.0, 0.0, 9.0 };
  double f[4] = { 12.0, 0.0, -9.5, 13.25 };

  u.x = _mm256_loadu2_m128d (e + 1, e + 5);
  if (check_union256d (u, f))
    abort ();
}
