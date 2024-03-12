/* { dg-do run } */
/* { dg-options "-O2  -msm3" } */
/* { dg-require-effective-target sm3 } */

#include "sm3-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static unsigned
p1 (unsigned w)
{
  return rol32 (w, 15) ^ rol32 (w, 23) ^ w;
}

static void
compute_sm3msg1 (int *src0, int *src1, int *src2, int *res)
{
  unsigned w0, w1, w2, w3, w7, w8, w9, w10, w13, w14, w15;

  w0 = src2[0];
  w1 = src2[1];
  w2 = src2[2];
  w3 = src2[3];
  w7 = src0[0];
  w8 = src0[1];
  w9 = src0[2];
  w10 = src0[3];
  w13 = src1[0];
  w14 = src1[1];
  w15 = src1[2];

  res[0] = p1 (w7 ^ w0 ^ rol32 (w13, 15));
  res[1] = p1 (w8 ^ w1 ^ rol32 (w14, 15));
  res[2] = p1 (w9 ^ w2 ^ rol32 (w15, 15));
  res[3] = p1 (w10 ^ w3);
}

static void
sm3_test (void)
{
  union128i_d s1, s2, s3, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 777, 888);
  s3.x = _mm_set_epi32 (999, 123, 456, 789);

  res.x = _mm_sm3msg1_epi32 (s1.x, s2.x, s3.x);

  compute_sm3msg1 (s1.a, s2.a, s3.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
