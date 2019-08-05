/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  union256 u;
  float e[8] = { 1.5f, -9.5f, 13.25f, -24.75f, -18.75f, 12.0f, 0.0f, 9.0f };
  float f[8] = { -24.75f, -18.75f, 12.0f, 0.0f, -9.5f, 13.25f, -24.75f, -18.75f };

  u.x = _mm256_loadu2_m128 (e + 1, e + 3);
  if (check_union256 (u, f))
    abort ();
}
