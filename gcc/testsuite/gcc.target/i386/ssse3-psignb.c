/* { dg-do run } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -fno-strict-aliasing -mssse3" } */

#include "ssse3-check.h"
#include "ssse3-vals.h"

#include <tmmintrin.h>

/* Test the 64-bit form */
static void
ssse3_test_psignb (int *i1, int *i2, int *r)
{
  __m64 t1 = *(__m64 *) i1;
  __m64 t2 = *(__m64 *) i2;
  *(__m64 *) r = _mm_sign_pi8 (t1, t2);
  _mm_empty ();
}

/* Test the 128-bit form */
static void
ssse3_test_psignb128 (int *i1, int *i2, int *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
  __m128i t1 = *(__m128i *) i1;
  __m128i t2 = *(__m128i *) i2;
  *(__m128i *) r = _mm_sign_epi8 (t1, t2);
}

/* Routine to manually compute the results */
static void
compute_correct_result (int *i1, int *i2, int *r)
{
  char *b1 = (char *) i1;
  char *b2 = (char *) i2;
  char *bout = (char *) r;
  int i;

  for (i = 0; i < 16; i++)
    if (b2[i] < 0)
      bout[i] = -b1[i];
    else if (b2[i] == 0)
      bout[i] = 0;
    else
      bout[i] = b1[i];
}

static void
ssse3_test (void)
{
  int i;
  int r [4] __attribute__ ((aligned(16)));
  int ck [4];
  int fail = 0;

  for (i = 0; i < 256; i += 8)
    {
      /* Manually compute the result */
      compute_correct_result (&vals[i + 0], &vals[i + 4], ck);

      /* Run the 64-bit tests */
      ssse3_test_psignb (&vals[i + 0], &vals[i + 4], &r[0]);
      ssse3_test_psignb (&vals[i + 2], &vals[i + 6], &r[2]);
      fail += chk_128 (ck, r);

      /* Run the 128-bit tests */
      ssse3_test_psignb128 (&vals[i + 0], &vals[i + 4], r);
      fail += chk_128 (ck, r);
    }

  if (fail != 0)
    abort ();
}
