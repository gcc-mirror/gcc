/* { dg-do run } */
/* { dg-options "-O2 -mavx512vp2intersect -mavx512vl" } */
/* { dg-require-effective-target avx512vp2intersect } */

#define AVX512VP2INTERSECT
#include <x86intrin.h>
#include "avx512f-helper.h"

 void
 TEST (void)
{
  __m256i a1 = _mm256_set_epi64x (1, 2, 3, 4);
  __m256i b1 = _mm256_set_epi64x (2, 11, 4, 33);
  __m256i a2 = _mm256_set_epi32 (1, 2, 3, 4, 5, 6, 7, 8);
  __m256i b2 = _mm256_set_epi32 (2, 11, 4, 33, 6, 55, 8, 77);
  __m128i a3 = _mm_set_epi64x (13, 22);
  __m128i b3 = _mm_set_epi64x (22, 1434);
  __m128i a4 = _mm_set_epi32 (1, 2, 3, 4);
  __m128i b4 = _mm_set_epi32 (2, 11, 4, 33);
  __mmask8 m0, m1, m2, m3, m4, m5, m6, m7;
  m0 = m1 = m2 = m3 = m4 = m5 = m6 = m7 = 0;

  _mm_2intersect_epi64 (a3, b3, &m0, &m1);
  /* m0 = ******01, m1 = ******10.  */
  if (m0 != 0x1 || m1 != 0x2)
    abort();

  _mm_2intersect_epi32 (a4, b4, &m2, &m3);
  /* m2 = ****0101, m3 = ****1010.  */
  if (m2  != 0x5 || m3 != 0xa)
    abort();

  _mm256_2intersect_epi64 (a1, b1, &m4, &m5);
  /* m4 = ****0101, m5 = ****1010.  */
  if (m4 != 0x5 || m5 != 0xa)
    abort();

  _mm256_2intersect_epi32 (a2, b2, &m6, &m7);
  /* m0 = 01010101, m1 = 10101010.  */
  if (m6 != 0x55 || m7 != 0xaa)
    abort();
}
