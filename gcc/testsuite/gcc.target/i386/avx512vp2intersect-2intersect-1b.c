/* { dg-do run } */
/* { dg-options "-O2 -mavx512vp2intersect" } */
/* { dg-require-effective-target avx512vp2intersect } */

#define AVX512VP2INTERSECT
#include <x86intrin.h>
#include "avx512f-helper.h"

void
TEST (void)
{
  __m512i a1 = _mm512_set_epi64 (10, 43, 253, 3566, 25, -253, -243, 3456);
  __m512i b1 = _mm512_set_epi64 (43, 100, 3566, 2353, -253, -25, 3456, 243);
  __m512i a2 = _mm512_set_epi32 (21, 22, 23, 24, 25, 26, 27, 28,
			       11, 12, 13, 14, 15, 16, 17, 18);
  __m512i b2 = _mm512_set_epi32 (22, 211, 24, 213, 26, 215, 28, 217,
				 12, 111, 14, 113, 16, 115, 18, 117);
  __mmask8 u8 = 0, m8 = 0;
  __mmask16 u16 = 0, m16 = 0;

  _mm512_2intersect_epi64 (a1, b1, &u8, &m8);
  /* u8 = 01010101, m8 = 10101010.  */
  if (u8 != 0x55 || m8 != 0xaa)
    abort();
  _mm512_2intersect_epi32 (a2, b2, &u16, &m16);
  /* u8 = 0101010101010101, m8 = 1010101010101010.  */
  if (u16 != 0x5555 || m16 != 0xaaaa)
    abort();
}
