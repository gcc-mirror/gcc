/* { dg-do run } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-require-effective-target vsx_hw } */

#define NO_WARN_X86_INTRINSICS 1
#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

#define DIM(a) (sizeof (a) / sizeof (a)[0])

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned short s[8];
    } src[] =
    {
      { .s = { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 } },
      { .s = { 0x0000, 0xffff, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 } },
      { .s = { 0xffff, 0xffff, 0x0000, 0xffff, 0xffff, 0xffff, 0x0000, 0xffff } },
      { .s = { 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008 } },
      { .s = { 0x0008, 0x0007, 0x0006, 0x0005, 0x0004, 0x0003, 0x0002, 0x0001 } },
      { .s = { 0xfff4, 0xfff3, 0xfff2, 0xfff1, 0xfff3, 0xfff1, 0xfff2, 0xfff3 } }
    };
  unsigned short minVal[DIM (src)];
  int minInd[DIM (src)];
  unsigned short minValScalar, minIndScalar;
  int i, j;
  union
    {
      int si;
      unsigned short s[2];
    } res;

  for (i = 0; i < DIM (src); i++)
    {
      res.si = _mm_cvtsi128_si32 (_mm_minpos_epu16 (src[i].x));
      minVal[i] = res.s[0];
      minInd[i] = res.s[1] & 0b111;
    }

  for (i = 0; i < DIM (src); i++)
    {
      minValScalar = src[i].s[0];
      minIndScalar = 0;

      for (j = 1; j < 8; j++)
	if (minValScalar > src[i].s[j])
	  {
	    minValScalar = src[i].s[j];
	    minIndScalar = j;
	  }

      if (minValScalar != minVal[i] && minIndScalar != minInd[i])
	abort ();
    }
}
