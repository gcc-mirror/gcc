/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_gatherdq (long long *res, __mmask8 m8, int *idx,
		  long long *src, int scale, long long *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      if (m8 & (1 << i))
	r[i] = *(long long *)
	  (((unsigned char *) src) + idx[i] * scale);
      else
	r[i] = res[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union256i_d idx;
  union512i_q res;
  long long src[8];
  long long res_ref[8];
  __mmask8 m8 = 0xC5;

  for (i = 0; i < 8; i++)
    {
      src[i] = 1983 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 8) >> 1;
    }

  res.x = _mm512_mask_i32gather_epi64 (res.x, m8, idx.x, src, SCALE);
  compute_gatherdq (res.a, m8, idx.a, src, SCALE, res_ref);

  if (check_union512i_q (res, res_ref))
    abort ();

  res.x = _mm512_i32gather_epi64 (idx.x, src, SCALE);
  compute_gatherdq (res.a, 0xFF, idx.a, src, SCALE, res_ref);

  if (check_union512i_q (res, res_ref))
    abort ();
}
