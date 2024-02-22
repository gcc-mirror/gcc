/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

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
ssse3_test_pmulhrsw (__m64 *i1, __m64 *i2, __m64 *r)
{
  *r = _mm_mulhrs_pi16 (*i1, *i2);
  _mm_empty ();
}
#endif

/* Test the 128-bit form */
static void
ssse3_test_pmulhrsw128 (__m128i *i1, __m128i *i2, __m128i *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
  *r = _mm_mulhrs_epi16 (*i1, *i2);
}

/* Routine to manually compute the results */
static void
compute_correct_result (short *i1, short *i2, short *r)
{
  int t0;
  int i;

  for (i = 0; i < 8; i++)
    {
      t0 = (((int) i1[i] * (int) i2[i]) >> 14) + 1;
      r[i] = (short) (t0 >> 1);
    }
}

static void
TEST (void)
{
  int i;
  union data r __attribute__ ((aligned(16)));
  union data ck;
  int fail = 0;

  for (i = 0; i < ARRAY_SIZE (vals) - 1; i++)
    {
      /* Manually compute the result */
      compute_correct_result (&vals[i + 0].h[0], &vals[i + 1].h[0], &ck.h[0]);

#ifndef __AVX__
      /* Run the 64-bit tests */
      ssse3_test_pmulhrsw (&vals[i + 0].ll[0], &vals[i + 1].ll[0], &r.ll[0]);
      ssse3_test_pmulhrsw (&vals[i + 0].ll[1], &vals[i + 1].ll[1], &r.ll[1]);
      fail += chk_128 (ck.m[0], r.m[0]);
#endif

      /* Run the 128-bit tests */
      ssse3_test_pmulhrsw128 (&vals[i + 0].m[0], &vals[i + 1].m[0], &r.m[0]);
      fail += chk_128 (ck.m[0], r.m[0]);
    }

  if (fail != 0)
    abort ();
}
