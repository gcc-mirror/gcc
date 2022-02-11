/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_i32gatherpd256 (long long *src,
			long long *s1,
			int *s2, long long *mask, int scale, long long *r)
{
  long long i;

  for (i = 0; i < 4; ++i)
    if ((mask[i] >> 63) & 1)
      r[i] = *(long long *) (((unsigned char *) s1) + s2[i] * scale);
    else
      r[i] = src[i];
}

void static
avx2_test (void)
{
  long long i;
  union128i_d idx;
  union256i_q res, src, mask;
  long long s1[16], res_ref[4] = { 0 };
  long long *s1_ptr = s1 + 8;

  for (i = 0; i < ARRAY_SIZE (s1); i++)
    {
      /* Set some stuff */
      s1[i] = 1983 * (i + 1) * (i + 2);
    }

  for (i = 0; i < 4; ++i)
    {
      /* Set src as something different from s1 */
      src.a[i] = -s1_ptr[i];

      /* Mask out evens */
      mask.a[i] = i % 2 ? 0 : -1;

      /* About to gather in reverse order, divide by 2
         to demonstrate scale */
      idx.a[i] = (16 - (i + 1) * 8) >> 1;
    }

  res.x = _mm256_mask_i32gather_epi64 (src.x,
				       (long long int *) s1_ptr,
				       idx.x, mask.x, 2);

  compute_i32gatherpd256 (src.a, s1_ptr, idx.a, mask.a, 2, res_ref);

  if (check_union256i_q (res, res_ref) != 0)
    abort ();
}
