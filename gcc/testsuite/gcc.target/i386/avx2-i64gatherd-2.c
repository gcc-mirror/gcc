/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_i64gatherps (int *s1, long long *s2, int scale, int *r)
{
  int i;

  for (i = 0; i < 2; ++i)
    r[i] = *(int *) (((unsigned char *) s1) + s2[i] * scale);
}

void static
avx2_test (void)
{
  int i;
  union128i_q idx;
  union128i_d res;
  int s1[2], res_ref[4] = { 0 };

  for (i = 0; i < 2; ++i)
    {
      /* Set some stuff */
      s1[i] = 1973 * (i + 1) * (i + 2);

      /* About to gather in reverse order,
         divide by 2 to demonstrate scale */
      idx.a[i] = (8 - (i + 1) * 4) >> 1;
    }

  res.x = _mm_i64gather_epi32 (s1, idx.x, 2);

  compute_i64gatherps (s1, idx.a, 2, res_ref);

  if (check_union128i_d (res, res_ref) != 0)
    abort ();
}
