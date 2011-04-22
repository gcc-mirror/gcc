/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

#ifndef MASK
#define MASK 0x7986
#endif

#define mask_v(pos) (((MASK & (0x1 << (pos))) >> (pos)) << 7)

void static
TEST (void)
{
  __m128i src, mask;
  char s[16] = { 1,-2,3,-4,5,-6,7,-8,9,-10,11,-12,13,-14,15,-16 };
  char m[16];

  char u[20] = { 0 };
  int i;

  for (i = 0; i < 16; i++)
    m[i] = mask_v (i);

  src = _mm_loadu_si128 ((__m128i *)s);
  mask = _mm_loadu_si128 ((__m128i *)m);

  _mm_maskmoveu_si128 (src, mask, u+3);

  for (i = 0; i < 16; i++)
    if (u[i+3] != (m[i] ? s[i] : 0))
      abort ();
}
