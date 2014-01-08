/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_scatterqd (__mmask8 m8, long long *idx,
		   int *src, int scale, int *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	*(int *) (((unsigned char *) r) + idx[i] * scale) = src[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union256i_d src;
  union512i_q idx;
  int res[8] = { 0 };
  int res_ref[8] = { 0 };
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src.a[i] = 1973 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (32 - (i + 1) * 4) >> 1;
    }

  _mm512_mask_i64scatter_epi32 (res, m8, idx.x, src.x, SCALE);
  compute_scatterqd (m8, idx.a, src.a, SCALE, res_ref);

  if (checkVi (res, res_ref, 8))
    abort ();

  _mm512_i64scatter_epi32 (res, idx.x, src.x, SCALE);
  compute_scatterqd (0xFF, idx.a, src.a, SCALE, res_ref);

  if (checkVi (res, res_ref, 8))
    abort ();
}
