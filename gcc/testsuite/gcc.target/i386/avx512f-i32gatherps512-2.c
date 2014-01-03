/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_gatherdps (float *res, __mmask16 m16, int *idx,
		   float *src, int scale, float *r)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      if (m16 & (1 << i))
	r[i] = *(float *) (((unsigned char *) src) + idx[i] * scale);
      else
	r[i] = res[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512 res;
  union512i_d idx;
  float src[16];
  float res_ref[16];
  __mmask16 m16 = 0xBC5D;

  res.x = _mm512_setzero_ps();

  for (i = 0; i < 16; i++)
    {
      src[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 4) >> 1;
    }

  res.x = _mm512_mask_i32gather_ps (res.x, m16, idx.x, src, SCALE);
  compute_gatherdps (res.a, m16, idx.a, src, SCALE, res_ref);

  if (check_union512 (res, res_ref))
    abort ();

  res.x = _mm512_i32gather_ps (idx.x, src, SCALE);
  compute_gatherdps (res.a, 0xFFFF, idx.a, src, SCALE, res_ref);

  if (check_union512 (res, res_ref))
    abort ();
}
