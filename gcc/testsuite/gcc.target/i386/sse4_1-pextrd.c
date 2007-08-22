/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

#define msk0   0
#define msk1   1
#define msk2   2
#define msk3   3

static void
sse4_1_test (void)
{
  union
    {
      __m128i x;
      int i[4];
    } val1;
  int res[4], masks[4];
  int i;

  val1.i[0] = 0x04030201;
  val1.i[1] = 0x08070605;
  val1.i[2] = 0x0C0B0A09;
  val1.i[3] = 0x100F0E0D;

  res[0] = _mm_extract_epi32 (val1.x, msk0);
  res[1] = _mm_extract_epi32 (val1.x, msk1);
  res[2] = _mm_extract_epi32 (val1.x, msk2);
  res[3] = _mm_extract_epi32 (val1.x, msk3);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;

  for (i = 0; i < 4; i++)
    if (res[i] != val1.i [masks[i]])
      abort ();
}
