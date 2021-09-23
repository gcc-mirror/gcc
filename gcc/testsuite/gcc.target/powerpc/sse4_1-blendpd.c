/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mpower8-vector -Wno-psabi" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>
#include <string.h>

#define NUM 20

#ifndef MASK
#define MASK 0x03
#endif

static void
init_blendpd (double *src1, double *src2)
{
  int i, sign = 1;

  for (i = 0; i < NUM * 2; i++)
    {
      src1[i] = i * i * sign;
      src2[i] = (i + 20) * sign;
      sign = -sign;
    }
}

static int
check_blendpd (__m128d *dst, double *src1, double *src2)
{
  double tmp[2];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));

  for(j = 0; j < 2; j++)
    if ((MASK & (1 << j)))
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
TEST (void)
{
  __m128d x, y;
  union
    {
      __m128d x[NUM];
      double d[NUM * 2];
    } dst, src1, src2;
  union
    {
      __m128d x;
      double d[2];
    } src3;
  int i;

  init_blendpd (src1.d, src2.d);

  /* Check blendpd imm8, m128, xmm */
  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blend_pd (src1.x[i], src2.x[i], MASK);
      if (check_blendpd (&dst.x[i], &src1.d[i * 2], &src2.d[i * 2]))
	abort ();
    }
    
  /* Check blendpd imm8, xmm, xmm */
  src3.x = _mm_setzero_pd ();

  x = _mm_blend_pd (dst.x[2], src3.x, MASK);
  y = _mm_blend_pd (src3.x, dst.x[2], MASK);

  if (check_blendpd (&x, &dst.d[4], &src3.d[0]))
    abort ();

  if (check_blendpd (&y, &src3.d[0], &dst.d[4]))
    abort ();
}
