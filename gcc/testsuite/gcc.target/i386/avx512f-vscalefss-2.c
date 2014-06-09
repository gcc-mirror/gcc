/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include <math.h>
#include "avx512f-check.h"

#define SIZE (128 / 32)

static void
compute_scalefss (float *s1, float *s2, float *r)
{
  r[0] = s1[0] * (float) pow (2, floor (s2[0]));
  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  union128 res1, s1, s2;
  float res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 11.5 * (i + 1);
      s2.a[i] = 10.5 * (i + 1);
    }

  res1.x = _mm_scalef_ss (s1.x, s2.x);

  compute_scalefss (s1.a, s2.a, res_ref);

  if (check_union128 (res1, res_ref))
    abort ();
}
