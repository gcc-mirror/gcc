/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_scatterdpd (__mmask8 m8, int *idx, double *src,
		    int scale, double *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	*(double *) (((unsigned char *) r) + idx[i] * scale) = src[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512d src;
  union256i_d idx;
  double res[8] = { 0.0 };
  double res_ref[8] = { 0.0 };
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src.a[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 8) >> 1;
    }

  _mm512_mask_i32scatter_pd (res, m8, idx.x, src.x, SCALE);
  compute_scatterdpd (m8, idx.a, src.a, SCALE, res_ref);

  if (checkVd (res, res_ref, 8))
    abort ();

  _mm512_i32scatter_pd (res, idx.x, src.x, SCALE);
  compute_scatterdpd (0xFF, idx.a, src.a, SCALE, res_ref);

  if (checkVd (res, res_ref, 8))
    abort ();
}
