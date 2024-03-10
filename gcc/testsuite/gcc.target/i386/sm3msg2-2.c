/* { dg-do run } */
/* { dg-options "-O2 -msm3" } */
/* { dg-require-effective-target sm3 } */

#include "sm3-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static void
compute_sm3msg2 (int *src0, int *src1, int *src2, int *res)
{
  unsigned wtmp0, wtmp1, wtmp2, wtmp3, w3, w4, w5, w6, w10, w11, w12, w13,
	   w16, w17, w18, w19;

  wtmp0 = src0[0];
  wtmp1 = src0[1];
  wtmp2 = src0[2];
  wtmp3 = src0[3];
  w3 = src1[0];
  w4 = src1[1];
  w5 = src1[2];
  w6 = src1[3];
  w10 = src2[0];
  w11 = src2[1];
  w12 = src2[2];
  w13 = src2[3];

  w16 = rol32 (w3, 7) ^ w10 ^ wtmp0;
  w17 = rol32 (w4, 7)  ^ w11 ^ wtmp1;
  w18 = rol32 (w5, 7)  ^ w12 ^ wtmp2;
  w19 = rol32 (w6, 7)  ^ w13 ^ wtmp3;

  w19 = w19 ^ rol32 (w16, 6) ^ rol32 (w16, 15)  ^ rol32 (w16, 30) ;

  res[0] = w16;
  res[1] = w17;
  res[2] = w18;
  res[3] = w19;
}

static void
sm3_test (void)
{
  union128i_d s1, s2, s3, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 777, 888);
  s3.x = _mm_set_epi32 (999, 123, 456, 789);

  res.x = _mm_sm3msg2_epi32 (s1.x, s2.x, s3.x);

  compute_sm3msg2 (s1.a, s2.a, s3.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
