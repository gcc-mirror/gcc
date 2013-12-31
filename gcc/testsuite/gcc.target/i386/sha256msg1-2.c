/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static int
s0 (int w)
{
  return __rord (w, 7) ^ __rord (w, 18) ^ (w >> 3);
}

static void
compute_sha256msg1 (int *src1, int *src2, int *res)
{
  int w0, w1, w2, w3, w4;

  w0 = src1[0];
  w1 = src1[1];
  w2 = src1[2];
  w3 = src1[3];
  w4 = src2[0];

  res[0] = w0 + s0 (w1);
  res[1] = w1 + s0 (w2);
  res[2] = w2 + s0 (w3);
  res[3] = w3 + s0 (w4);
}

static void
sha_test (void)
{
  union128i_d s1, s2, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (0, 0, 0, 555);

  res.x = _mm_sha256msg1_epu32 (s1.x, s2.x);

  compute_sha256msg1 (s1.a, s2.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
