/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

#define NUM 64

static void
TEST (void)
{
  union
    {
      __m128i x[NUM/8];
      unsigned short s[NUM];
    } src;
  unsigned short minVal[NUM/8];
  int minInd[NUM/8];
  unsigned short minValScalar, minIndScalar;
  int i, j, res;

  for (i = 0; i < NUM; i++)
    src.s[i] = i * i / (i + i / 3.14 + 1.0);

  for (i = 0, j = 0; i < NUM; i += 8, j++)
    {
      res = _mm_cvtsi128_si32 (_mm_minpos_epu16 (src.x [i/8]));
      minVal[j] = res & 0xffff;
      minInd[j] = (res >> 16) & 0x3;
    }

  for (i = 0; i < NUM; i += 8)
    {
      minValScalar = src.s[i];
      minIndScalar = 0;

      for (j = i + 1; j < i + 8; j++)
	if (minValScalar > src.s[j])
	  {
	    minValScalar = src.s[j];
	    minIndScalar = j - i;
	  }

      if (minValScalar != minVal[i/8] && minIndScalar != minInd[i/8])
	abort ();
    }
}
