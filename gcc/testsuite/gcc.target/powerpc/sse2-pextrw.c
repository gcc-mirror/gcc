/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pextrw_1
#endif

#include <emmintrin.h>

#define msk0   0
#define msk1   1
#define msk2   2
#define msk3   3
#define msk4   4
#define msk5   5
#define msk6   6
#define msk7   7

static void
TEST (void)
{
  union
    {
      __m128i x;
      int i[4];
      short s[8];
    } val1;
  int res[8], masks[8];
  int i;

  val1.i[0] = 0x04030201;
  val1.i[1] = 0x08070605;
  val1.i[2] = 0x0C0B0A09;
  val1.i[3] = 0x100F0E0D;

  res[0] = _mm_extract_epi16 (val1.x, msk0);
  res[1] = _mm_extract_epi16 (val1.x, msk1);
  res[2] = _mm_extract_epi16 (val1.x, msk2);
  res[3] = _mm_extract_epi16 (val1.x, msk3);
  res[4] = _mm_extract_epi16 (val1.x, msk4);
  res[5] = _mm_extract_epi16 (val1.x, msk5);
  res[6] = _mm_extract_epi16 (val1.x, msk6);
  res[7] = _mm_extract_epi16 (val1.x, msk7);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;

  for (i = 0; i < 8; i++)
    if (res[i] != val1.s [masks[i]])
      abort ();
}
