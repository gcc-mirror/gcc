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
ssse3_test_pshufb (int *i1, int *i2, int *r)
{
  __m64 t1 = *(__m64 *) i1;
  __m64 t2 = *(__m64 *) i2;
  *(__m64 *)r = _mm_shuffle_pi8 (t1, t2);
  _mm_empty ();
}
#endif

/* Test the 128-bit form */
static void
ssse3_test_pshufb128 (int *i1, int *i2, int *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
  __m128i t1 = *(__m128i *) i1;
  __m128i t2 = *(__m128i *) i2;
  *(__m128i *)r = _mm_shuffle_epi8 (t1, t2);
}

#ifndef __AVX__
/* Routine to manually compute the results */
static void
compute_correct_result_64 (int *i1, int *i2, int *r)
{
  char *b1 = (char *) i1;
  char *b2 = (char *) i2;
  char *bout = (char *) r;
  int i;
  char select;

  for (i = 0; i < 16; i++)
    {
      select = b2[i];
      if (select & 0x80)
	bout[i] = 0;
      else if (i < 8)
	bout[i] = b1[select & 0x7];
      else
	bout[i] = b1[8 + (select & 0x7)];
    }
}
#endif

static void
compute_correct_result_128 (int *i1, int *i2, int *r)
{
  char *b1 = (char *) i1;
  char *b2 = (char *) i2;
  char *bout = (char *) r;
  int i;
  char select;

  for (i = 0; i < 16; i++)
    {
      select = b2[i];
      if (select & 0x80)
	bout[i] = 0;
      else
	bout[i] = b1[select & 0xf];
    }
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
#ifndef __AVX__
      /* Manually compute the result */
      compute_correct_result_64 (&vals[i + 0], &vals[i + 4], ck);

      /* Run the 64-bit tests */
      ssse3_test_pshufb (&vals[i + 0], &vals[i + 4], &r[0]);
      ssse3_test_pshufb (&vals[i + 2], &vals[i + 6], &r[2]);
      fail += chk_128 (ck, r);
#endif

      /* Recompute the result for 128-bits */
      compute_correct_result_128 (&vals[i + 0], &vals[i + 4], ck);

      /* Run the 128-bit tests */
      ssse3_test_pshufb128 (&vals[i + 0], &vals[i + 4], r);
      fail += chk_128 (ck, r);
    }

  if (fail != 0)
    abort ();
}
