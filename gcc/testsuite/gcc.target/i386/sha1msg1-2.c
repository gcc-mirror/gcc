/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <immintrin.h>

static void
compute_sha1msg1 (int *s1, int *s2, int *r)
{
  int w0, w1, w2, w3, w4, w5;

  w0 = s1[3];
  w1 = s1[2];
  w2 = s1[1];
  w3 = s1[0];
  w4 = s2[3];
  w5 = s2[2];

  r[0] = w5 ^ w3;
  r[1] = w4 ^ w2;
  r[2] = w3 ^ w1;
  r[3] = w2 ^ w0;
}

static void
sha_test (void)
{
  union128i_d s1, s2, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 0, 0);

  res.x = _mm_sha1msg1_epu32 (s1.x, s2.x);

  compute_sha1msg1 (s1.a, s2.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
