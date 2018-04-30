/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -mtune=knl" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define SCALE 2

static void
compute_gatherdd (int *res, __mmask16 m16, int *idx,
		  int *src, int scale, int *r)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      if (m16 & (1 << i))
	r[i] = *(int *) (((unsigned char *) src) + idx[i] * scale);
      else
	r[i] = res[i];
    }
}

static void
avx512f_test (void)
{
  int i;
  union512i_d idx, res;
  int src[16];
  int res_ref[16];
  __mmask16 m16 = 0xBC5D;

  for (i = 0; i < 16; i++)
    {
      src[i] = 1973 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
	 divide by 2 to demonstrate scale */
      idx.a[i] = (64 - (i + 1) * 4) >> 1;
    }

  res.x = _mm512_mask_i32gather_epi32 (res.x, m16, idx.x, src, SCALE);
  compute_gatherdd (res.a, m16, idx.a, src, SCALE, res_ref);

  if (check_union512i_d (res, res_ref))
    abort ();

  res.x = _mm512_i32gather_epi32 (idx.x, src, SCALE);
  compute_gatherdd (res.a, 0xFFFF, idx.a, src, SCALE, res_ref);

  if (check_union512i_d (res, res_ref))
    abort ();
}
