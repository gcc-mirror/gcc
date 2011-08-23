/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "ssse3-vals.h"
#include "avx2-check.h"

/* Routine to manually compute the results */
static void
compute_pabs256 (int *i1, short *r)
{
  short *b1 = (short *) i1;
  int i;

  for (i = 0; i < 16; i++)
    if (b1[i] < 0)
      r[i] = -b1[i];
    else
      r[i] = b1[i];
}

static void
avx2_test (void)
{
  int i;
  short ck[16];
  int fail = 0;

  union256i_w s, d;

  for (i = 0; i < 256; ++i)
    {
      /* Using only first 2 bytes of int */
      /* Recompute the results for 256-bits */
      compute_pabs256 (&vals[i], ck);

      s.x = _mm256_loadu_si256 ((__m256i *) & vals[i]);

      /* Run the 256-bit tests */
      d.x = _mm256_abs_epi16 (s.x);

      fail += check_union256i_w (d, ck);
    }

  if (fail != 0)
    abort ();
}
