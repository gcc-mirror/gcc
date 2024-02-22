/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>

#define NUM 20

static void
init_blendvpd (double *src1, double *src2, double *mask)
{
  int i, msk, sign = 1; 

  msk = -1;
  for (i = 0; i < NUM * 2; i++)
    {
      if((i % 2) == 0)
	msk++;
      src1[i] = i* (i + 1) * sign;
      src2[i] = (i + 20) * sign;
      mask[i] = (i + 120) * i;
      if( (msk & (1 << (i % 2))))
	mask[i] = -mask[i];
      sign = -sign;
    }
}

static int
check_blendvpd (__m128d *dst, double *src1, double *src2,
		double *mask)
{
  double tmp[2];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));
  for (j = 0; j < 2; j++)
    if (mask [j] < 0.0)
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
sse4_1_test (void)
{
  union
    {
      __m128d x[NUM];
      double d[NUM * 2];
    } dst, src1, src2, mask;
  int i;

  init_blendvpd (src1.d, src2.d, mask.d);

  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blendv_pd (src1.x[i], src2.x[i], mask.x[i]);
      if (check_blendvpd (&dst.x[i], &src1.d[i * 2], &src2.d[i * 2],
			  &mask.d[i * 2]))
	abort ();
    }
}
