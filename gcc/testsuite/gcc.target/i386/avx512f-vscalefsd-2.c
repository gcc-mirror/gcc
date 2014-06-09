/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include <math.h>
#include "avx512f-check.h"

#define SIZE (128 / 64)

static void
compute_scalefsd (double *s1, double *s2, double *r)
{
  r[0] = s1[0] * pow (2, floor (s2[0]));
  r[1] = s1[1];
}

void static
avx512f_test (void)
{
  union128d res1, s1, s2;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 11.5 * (i + 1);
      s2.a[i] = 10.5 * (i + 1);
    }

  res1.x = _mm_scalef_sd (s1.x, s2.x);

  compute_scalefsd (s1.a, s2.a, res_ref);

  if (check_union128d (res1, res_ref))
    abort ();
}
