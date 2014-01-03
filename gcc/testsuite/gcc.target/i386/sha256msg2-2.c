/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static int
s1 (int w)
{
  return __rord (w, 17) ^ __rord (w, 19) ^ (w >> 10);
}

static void
compute_sha256msg2 (int *src1, int *src2, int *res)
{
  int w14, w15, w16, w17, w18, w19;

  w14 = src2[2];
  w15 = src2[3];
  w16 = src1[0] + s1 (w14);
  w17 = src1[1] + s1 (w15);
  w18 = src1[2] + s1 (w16);
  w19 = src1[3] + s1 (w17);

  res[0] = w16;
  res[1] = w17;
  res[2] = w18;
  res[3] = w19;
}

static void
sha_test (void)
{
  union128i_d s1, s2, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 0, 0);

  res.x = _mm_sha256msg2_epu32 (s1.x, s2.x);

  compute_sha256msg2 (s1.a, s2.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
