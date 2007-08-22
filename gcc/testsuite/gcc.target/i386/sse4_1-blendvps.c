/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>

#define NUM 20

static void
init_blendvps (float *src1, float *src2, float *mask)
{
  int i, msk, sign = 1; 

  msk = -1;
  for (i = 0; i < NUM * 4; i++)
    {
      if((i % 4) == 0)
	msk++;
      src1[i] = i* (i + 1) * sign;
      src2[i] = (i + 20) * sign;
      mask[i] = (i + 120) * i;
      if( (msk & (1 << (i % 4))))
	mask[i] = -mask[i];
      sign = -sign;
    }
}

static int
check_blendvps (__m128 *dst, float *src1, float *src2,
		float *mask)
{
  float tmp[4];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));
  for (j = 0; j < 4; j++)
    if (mask [j] < 0.0)
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
sse4_1_test (void)
{
  union
    {
      __m128 x[NUM];
      float f[NUM * 4];
    } dst, src1, src2, mask;
  int i;

  init_blendvps (src1.f, src2.f, mask.f);

  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blendv_ps (src1.x[i], src2.x[i], mask.x[i]);
      if (check_blendvps (&dst.x[i], &src1.f[i * 4], &src2.f[i * 4],
			  &mask.f[i * 4]))
	abort ();
    }
}
