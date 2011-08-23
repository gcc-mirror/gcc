/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_i32gatherpd (long long *s1, int *s2, int scale, long long *r)
{
  long long i;

  for (i = 0; i < 2; ++i)
    r[i] = *(long long *) (((unsigned char *) s1) + s2[i] * scale);
}

void static
avx2_test (void)
{
  long long i;
  union128i_d idx;
  union128i_q res;
  long long s1[2], res_ref[2] = { 0 };

  for (i = 0; i < 2; ++i)
    {
      /* Set some stuff */
      s1[i] = 1983 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (16 - (i + 1) * 8) >> 1;
    }

  res.x = _mm_i32gather_epi64 ((long long int *) s1, idx.x, 2);

  compute_i32gatherpd (s1, idx.a, 2, res_ref);

  if (check_union128i_q (res, res_ref) != 0)
    abort ();
}
