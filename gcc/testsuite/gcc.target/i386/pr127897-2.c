/* PR target/127897 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx -mtune-ctrl=^prefer_bcst_from_integer" } */
/* { dg-final { scan-assembler-not "pshufd" } } */
/* { dg-final { scan-assembler-not "movd\[ \\t\]" } } */

#include <immintrin.h>

__m128i
f (__m128i x)
{
  return _mm_add_epi32 (_mm_mullo_epi32 (x, _mm_set1_epi32 (12345)),
			_mm_set1_epi32 (678));
}
