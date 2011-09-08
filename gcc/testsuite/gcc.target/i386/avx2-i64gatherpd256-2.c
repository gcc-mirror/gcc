/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_i64gatherpd256 (double *s1, long long int *s2, int scale, double *r)
{
  int i;

  for (i = 0; i < 4; ++i)
    r[i] = *(double *) (((unsigned char *) s1) + s2[i] * scale);
}

void static
avx2_test (void)
{
  int i;
  union256i_q idx;
  union256d res;
  double s1[4], res_ref[4] = { 0 };

  for (i = 0; i < 4; ++i)
    {
      /* Set some stuff */
      s1[i] = 2.718281828459045 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (32 - (i + 1) * 8) >> 1;
    }

  res.x = _mm256_i64gather_pd (s1, idx.x, 2);

  compute_i64gatherpd256 (s1, idx.a, 2, res_ref);

  if (check_union256d (res, res_ref) != 0)
    abort ();
}
