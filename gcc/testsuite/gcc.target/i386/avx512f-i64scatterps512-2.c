/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_scatterqps (__mmask8 m8, long long *idx,
		    float *src, int scale, float *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	*(float *) (((unsigned char *) r) + idx[i] * scale) = src[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union256 src;
  union512i_q idx;
  float res[8] = { 0.0 };
  float res_ref[8] = { 0.0 };
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src.a[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (32 - (i + 1) * 4) >> 1;
    }

  _mm512_mask_i64scatter_ps (res, m8, idx.x, src.x, SCALE);
  compute_scatterqps (m8, idx.a, src.a, SCALE, res_ref);

  if (checkVf (res, res_ref, 8))
    abort ();

  _mm512_i64scatter_ps (res, idx.x, src.x, SCALE);
  compute_scatterqps (0xFF, idx.a, src.a, SCALE, res_ref);

  if (checkVf (res, res_ref, 8))
    abort ();
}
