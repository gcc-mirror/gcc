/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#define SIZE (128 / 64)

#include <math.h>
#include "avx512f-check.h"

static void
compute_vgetexpsd (double *s, double *r)
{
  r[0] = floor (log (s[0]) / log (2));
}

void static
avx512f_test (void)
{
  int i;
  union128d res1, s1;
  double res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 5.0 - i;
      res_ref[i] = s1.a[i];
    }

  res1.x = _mm_getexp_sd (s1.x, s1.x);

  compute_vgetexpsd (s1.a, res_ref);

  if (check_fp_union128d (res1, res_ref))
    abort ();
}
