/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
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
ssse3_test_phaddsw (__m64 *i1, __m64 *i2, __m64 *r)
{
  *r = _mm_hadds_pi16 (*i1, *i2);
  _mm_empty ();
}
#endif

/* Test the 128-bit form */
static void
ssse3_test_phaddsw128 (__m128i *i1, __m128i *i2, __m128i *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
 *(__m128i *) r = _mm_hadds_epi16 (*i1, *i2);
}

static short
signed_saturate_to_word (int x)
{
  if (x > (int) 0x7fff)
    return 0x7fff;

  if (x < (int) 0xffff8000)
    return 0x8000;

  return (short) x;
}

/* Routine to manually compute the results */
static void
compute_correct_result (short *i1, short *i2, short *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i + 0] = signed_saturate_to_word(i1[2 * i] + i1[2 * i + 1]);
  for (i = 0; i < 4; i++)
    r[i + 4] = signed_saturate_to_word(i2[2 * i] + i2[2 * i + 1]);
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
      ssse3_test_phaddsw (&vals[i + 0].ll[0], &vals[i + 0].ll[1], &r.ll[0]);
      ssse3_test_phaddsw (&vals[i + 1].ll[0], &vals[i + 1].ll[1], &r.ll[1]);
      fail += chk_128 (ck.m[0], r.m[0]);
#endif

      /* Run the 128-bit tests */
      ssse3_test_phaddsw128 (&vals[i + 0].m[0], &vals[i + 1].m[0], &r.m[0]);
      fail += chk_128 (ck.m[0], r.m[0]);
    }

  if (fail != 0)
    abort ();
}
