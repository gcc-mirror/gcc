/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_i64gatherpd256 (double *src,
			double *s1,
			long long int *s2, double *mask, int scale, double *r)
{
  int i;

  for (i = 0; i < 4; ++i)
    if ((((long long *) mask)[i] >> 63) & 1)
      r[i] = *(double *) (((unsigned char *) s1) + s2[i] * scale);
    else
      r[i] = src[i];
}

void static
avx2_test (void)
{
  int i;
  union256i_q idx;
  union256d res, src, mask;
  double s1[16], res_ref[4] = { 0 };
  double *s1_ptr = s1 + 8;

  for (i = 0; i < ARRAY_SIZE (s1); i++)
    {
      /* Set some stuff */
      s1[i] = 2.718281828459045 * (i + 1) * (i + 2);
    }

  for (i = 0; i < 4; ++i)
    {
      /* Set src as something different from s1 */
      src.a[i] = -s1_ptr[i];

      /* Mask out evens */
      ((long long *) mask.a)[i] = i % 2 ? 0 : -1;

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (16 - (i + 1) * 8) >> 1;
    }

  res.x = _mm256_mask_i64gather_pd (src.x, s1_ptr, idx.x, mask.x, 2);

  compute_i64gatherpd256 (src.a, s1_ptr, idx.a, mask.a, 2, res_ref);

  if (check_union256d (res, res_ref) != 0)
    abort ();
}
