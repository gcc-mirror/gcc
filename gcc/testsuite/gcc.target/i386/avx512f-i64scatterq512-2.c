/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_scatterqq (__mmask8 m8, long long *idx, long long *src,
		   int scale, long long *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	*(long long *) (((unsigned char *) r) + idx[i] * scale) =
	  src[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512i_q src, idx;
  long long res[8] = { 0 };
  long long res_ref[8] = { 0 };
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src.a[i] = 1983 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 8) >> 1;
    }

  _mm512_mask_i64scatter_epi64 (res, m8, idx.x, src.x, SCALE);
  compute_scatterqq (m8, idx.a, src.a, SCALE, res_ref);

  if (checkVl (res, res_ref, 8))
    abort ();

  _mm512_i64scatter_epi64 (res, idx.x, src.x, SCALE);
  compute_scatterqq (0xFF, idx.a, src.a, SCALE, res_ref);

  if (checkVl (res, res_ref, 8))
    abort ();
}
