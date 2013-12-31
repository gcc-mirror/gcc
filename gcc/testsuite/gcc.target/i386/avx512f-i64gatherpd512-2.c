/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_gatherqpd (double *res, __mmask8 m8, long long *idx,
		   double *src, int scale, double *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	r[i] = *(double *) (((unsigned char *) src) + idx[i] * scale);
      else
	r[i] = res[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512d res;
  union512i_q idx;
  double src[8];
  double res_ref[8];
  __mmask8 m8 = 0xC5;

  res.x = _mm512_setzero_pd();

  for (i = 0; i < 8; i++)
    {
      src[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 8) >> 1;
    }

  res.x = _mm512_mask_i64gather_pd (res.x, m8, idx.x, src, SCALE);
  compute_gatherqpd (res.a, m8, idx.a, src, SCALE, res_ref);

  if (check_union512d (res, res_ref))
    abort ();

  res.x = _mm512_i64gather_pd (idx.x, src, SCALE);
  compute_gatherqpd (res.a, 0xFF, idx.a, src, SCALE, res_ref);

  if (check_union512d (res, res_ref))
    abort ();
}
