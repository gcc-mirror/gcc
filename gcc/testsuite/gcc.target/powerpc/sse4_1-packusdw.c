/* { dg-do run } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target vsx_hw } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

#define NUM 64

static unsigned short
int_to_ushort (int iVal)
{
  unsigned short sVal;

  if (iVal < 0)
    sVal = 0;
  else if (iVal > 0xffff)
    sVal = 0xffff;
  else sVal = iVal;

  return sVal;
}

static void
TEST (void)
{
  union
    {
      __m128i x[NUM / 4];
      int i[NUM];
    } src1, src2;
  union
    {
      __m128i x[NUM / 4];
      unsigned short s[NUM * 2];
    } dst;
  int i, sign = 1;

  for (i = 0; i < NUM; i++)
    {
      src1.i[i] = i * i * sign;
      src2.i[i] = (i + 20) * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 4)
    dst.x[i / 4] = _mm_packus_epi32 (src1.x [i / 4], src2.x [i / 4]);

  for (i = 0; i < NUM; i ++)
    {
      int dstIndex;
      unsigned short sVal;

      sVal = int_to_ushort (src1.i[i]);
      dstIndex = (i % 4) + (i / 4) * 8;
      if (sVal != dst.s[dstIndex])
	abort ();

      sVal = int_to_ushort (src2.i[i]);
      dstIndex += 4;
      if (sVal != dst.s[dstIndex])
	abort ();
    }
}
