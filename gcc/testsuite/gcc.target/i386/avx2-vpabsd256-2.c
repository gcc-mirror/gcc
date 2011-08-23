/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "ssse3-vals.h"
#include "avx2-check.h"

/* Routine to manually compute the results */
static void
compute_pabs256 (int *i1, int *r)
{
  int i;

  for (i = 0; i < 8; i++)
    if (i1[i] < 0)
      r[i] = -i1[i];
    else
      r[i] = i1[i];
}

static void
avx2_test (void)
{
  int i;
  int ck[8];
  int fail = 0;

  union256i_d s, d;

  for (i = 0; i < 256; ++i)
    {
      /* Recompute the results for 256-bits */
      compute_pabs256 (&vals[i], ck);

      s.x = _mm256_loadu_si256 ((__m256i *) & vals[i]);

      /* Run the 256-bit tests */
      d.x = _mm256_abs_epi32 (s.x);

      fail += check_union256i_d (d, ck);
    }

  if (fail != 0)
    abort ();
}
