/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vp2intersect -mavx512vl" } */
/* { dg-final { scan-assembler "vp2intersectd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\]"  } } */
/* { dg-final { scan-assembler "vp2intersectd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\]"  } } */
/* { dg-final { scan-assembler "vp2intersectq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\]"  } } */
/* { dg-final { scan-assembler "vp2intersectq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\]"  } } */

#include <x86intrin.h>

__m256i a2, b2;
__m128i a3, b3;
__mmask8 m0, m1, m2, m3, m4, m5, m6, m7;

int foo ()
{
  _mm_2intersect_epi64 (a3, b3, &m0, &m1);
  _mm_2intersect_epi32 (a3, b3, &m2, &m3);

  _mm256_2intersect_epi64 (a2, b2, &m4, &m5);
  _mm256_2intersect_epi32 (a2, b2, &m6, &m7);
}
