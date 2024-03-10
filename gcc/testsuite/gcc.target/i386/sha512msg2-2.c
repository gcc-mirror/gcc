/* { dg-do run } */
/* { dg-options "-O2 -msha512" } */
/* { dg-require-effective-target sha512 } */

#include "sha512-check.h"
#include <immintrin.h>

static unsigned long long
s1 (unsigned long long w)
{
  return ror64 (w, 19) ^ ror64 (w, 61) ^ shr64 (w, 6);
}

static void
compute_sha512msg2 (long long* src1, long long* src2, long long* res)
{
  unsigned long long w14, w15, w16, w17, w18, w19;

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
sha512_test (void)
{
  union256i_q s1, s2, res;
  long long res_ref[4];

  s1.x = _mm256_set_epi64x (111, 222, 333, 444);
  s2.x = _mm256_set_epi64x (555, 666, 0, 0);

  res.x = _mm256_sha512msg2_epi64 (s1.x, s2.x);

  compute_sha512msg2 (s1.a, s2.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort();
}
