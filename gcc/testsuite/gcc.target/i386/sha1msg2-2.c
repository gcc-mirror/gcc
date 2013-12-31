/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static void
compute_sha1msg2 (int *s1, int *s2, int *r)
{
  int w13, w14, w15, w16, w17, w18, w19;

  w13 = s2[2];
  w14 = s2[1];
  w15 = s2[0];
  w16 = __rold (s1[3] ^ w13, 1);
  w17 = __rold (s1[2] ^ w14, 1);
  w18 = __rold (s1[1] ^ w15, 1);
  w19 = __rold (s1[0] ^ w16, 1);

  r[0] = w19;
  r[1] = w18;
  r[2] = w17;
  r[3] = w16;
}

static void
sha_test (void)
{
  union128i_d s1, s2, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 777, 0);

  res.x = _mm_sha1msg2_epu32 (s1.x, s2.x);

  compute_sha1msg2 (s1.a, s2.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
