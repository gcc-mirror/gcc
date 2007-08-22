/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

#define msk0   0
#define msk1   1
#define msk2   2
#define msk3   3
#define msk4   4
#define msk5   5
#define msk6   6
#define msk7   7
#define msk8   8
#define msk9   9
#define msk10 10
#define msk11 11
#define msk12 12
#define msk13 13
#define msk14 14
#define msk15 15

static void
sse4_1_test (void)
{
  union
    {
      __m128i x;
      int i[4];
      char c[16];
    } val1;
  int res[16], masks[16];
  int i;

  val1.i[0] = 0x04030201;
  val1.i[1] = 0x08070605;
  val1.i[2] = 0x0C0B0A09;
  val1.i[3] = 0x100F0E0D;

  res[0] = _mm_extract_epi8 (val1.x, msk0);
  res[1] = _mm_extract_epi8 (val1.x, msk1);
  res[2] = _mm_extract_epi8 (val1.x, msk2);
  res[3] = _mm_extract_epi8 (val1.x, msk3);
  res[4] = _mm_extract_epi8 (val1.x, msk4);
  res[5] = _mm_extract_epi8 (val1.x, msk5);
  res[6] = _mm_extract_epi8 (val1.x, msk6);
  res[7] = _mm_extract_epi8 (val1.x, msk7);
  res[8] = _mm_extract_epi8 (val1.x, msk8);
  res[9] = _mm_extract_epi8 (val1.x, msk9);
  res[10] = _mm_extract_epi8 (val1.x, msk10);
  res[11] = _mm_extract_epi8 (val1.x, msk11);
  res[12] = _mm_extract_epi8 (val1.x, msk12);
  res[13] = _mm_extract_epi8 (val1.x, msk13);
  res[14] = _mm_extract_epi8 (val1.x, msk14);
  res[15] = _mm_extract_epi8 (val1.x, msk15);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;
  masks[8] = msk8;
  masks[9] = msk9;
  masks[10] = msk10;
  masks[11] = msk11;
  masks[12] = msk12;
  masks[13] = msk13;
  masks[14] = msk14;
  masks[15] = msk15;

  for (i = 0; i < 16; i++)
    if (res[i] != val1.c [masks[i]])
      abort ();
}
