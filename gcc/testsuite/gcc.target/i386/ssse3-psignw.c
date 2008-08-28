/* { dg-do run } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -fno-strict-aliasing -mssse3" } */

#ifndef CHECK_H
#define CHECK_H "ssse3-check.h"
#endif

#ifndef TEST
#define TEST ssse3_test
#endif

#include CHECK_H
#include "ssse3-vals.h"

#include <tmmintrin.h>

#ifndef __AVX__
/* Test the 64-bit form */
static void
ssse3_test_psignw (int *i1, int *i2, int *r)
{
  __m64 t1 = *(__m64 *) i1;
  __m64 t2 = *(__m64 *) i2;
  *(__m64 *) r = _mm_sign_pi16 (t1, t2);
  _mm_empty ();
}
#endif

/* Test the 128-bit form */
static void
ssse3_test_psignw128 (int *i1, int *i2, int *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
  __m128i t1 = *(__m128i *) i1;
  __m128i t2 = *(__m128i *) i2;
 *(__m128i *) r = _mm_sign_epi16 (t1, t2);
}

/* Routine to manually compute the results */
static void
compute_correct_result (int *i1, int *i2, int *r)
{
  short *s1 = (short *) i1;
  short *s2 = (short *) i2;
  short *sout = (short *) r;
  int i;

  for (i = 0; i < 8; i++)
    if (s2[i] < 0)
      sout[i] = -s1[i];
    else if (s2[i] == 0)
      sout[i] = 0;
    else
      sout[i] = s1[i];
}

static void
TEST (void)
{
  int i;
  int r [4] __attribute__ ((aligned(16)));
  int ck [4];
  int fail = 0;

  for (i = 0; i < 256; i += 8)
    {
      /* Manually compute the result */
      compute_correct_result (&vals[i + 0], &vals[i + 4], ck);

#ifndef __AVX__
      /* Run the 64-bit tests */
      ssse3_test_psignw (&vals[i + 0], &vals[i + 4], &r[0]);
      ssse3_test_psignw (&vals[i + 2], &vals[i + 6], &r[2]);
      fail += chk_128 (ck, r);
#endif

      /* Run the 128-bit tests */
      ssse3_test_psignw128 (&vals[i + 0], &vals[i + 4], r);
      fail += chk_128 (ck, r);
    }

  if (fail != 0)
    abort ();
}
