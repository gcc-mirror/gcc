/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_gatherqd (int *res, __mmask8 m8, long long *idx,
		  int *src, int scale, int *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	r[i] = *(int *) (((unsigned char *) src) + idx[i] * scale);
      else
	r[i] = res[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union256i_d res;
  union512i_q idx;
  int src[8];
  int res_ref[8];
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src[i] = 1973 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (32 - (i + 1) * 4) >> 1;
    }

  res.x = _mm512_mask_i64gather_epi32 (res.x, m8, idx.x, src, SCALE);
  compute_gatherqd (res.a, m8, idx.a, src, SCALE, res_ref);

  if (check_union256i_d (res, res_ref))
    abort ();

  res.x = _mm512_i64gather_epi32 (idx.x, src, SCALE);
  compute_gatherqd (res.a, 0xFF, idx.a, src, SCALE, res_ref);

  if (check_union256i_d (res, res_ref))
    abort ();
}
