/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_scatterdps (__mmask16 m16, int *idx,
		    float *src, int scale, float *r)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      if (m16 & (1 << i))
	*(float *) (((unsigned char *) r) + idx[i] * scale) = src[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512 src;
  union512i_d idx;
  float res[16] = { 0.0 };
  float res_ref[16] = { 0.0 };
  __mmask16 m16 = 0xBC5D;

  for (i = 0; i < 16; i++)
    {
      src.a[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 4) >> 1;
    }

  _mm512_mask_i32scatter_ps (res, m16, idx.x, src.x, SCALE);
  compute_scatterdps (m16, idx.a, src.a, SCALE, res_ref);

  if (checkVf (res, res_ref, 16))
    abort ();

  _mm512_i32scatter_ps (res, idx.x, src.x, SCALE);
  compute_scatterdps (0xFFFF, idx.a, src.a, SCALE, res_ref);

  if (checkVf (res, res_ref, 16))
    abort ();
}
