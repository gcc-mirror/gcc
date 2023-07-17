/* { dg-do run } */
/* { dg-options "-O2 -msha512" } */
/* { dg-require-effective-target sha512 } */

#include "sha512-check.h"
#include <emmintrin.h>
#include <immintrin.h>

static unsigned long long
s0 (unsigned long long w)
{
  return ror64 (w, 1) ^ ror64 (w, 8) ^ shr64 (w, 7);
}

static void
compute_sha512msg1(long long* src1, long long* src2, long long* res)
{
  unsigned long long w0, w1, w2, w3, w4;

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
sha512_test(void)
{
  union256i_q s1, res;
  union128i_q s2;
  long long res_ref[4];

  s1.x = _mm256_set_epi64x (111, 222, 333, 444);
  s2.x = _mm_set_epi64x (0, 555);

  res.x = _mm256_sha512msg1_epi64 (s1.x, s2.x);

  compute_sha512msg1 (s1.a, s2.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort();
}
