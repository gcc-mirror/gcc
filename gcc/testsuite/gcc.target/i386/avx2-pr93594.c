/* PR target/93594 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -masm=att" } */
/* { dg-final { scan-assembler-times "vmovdqa\t%xmm0, %xmm0" 4 } } */
/* { dg-final { scan-assembler-not "vpxor\t%" } } */
/* { dg-final { scan-assembler-not "vinserti128\t\\\$" } } */

#include <x86intrin.h>

__m256i
foo (__m128i x)
{
  return _mm256_setr_m128i (x, _mm_setzero_si128 ());
}

__m256i
bar (__m128i x)
{
  return _mm256_set_m128i (_mm_setzero_si128 (), x);
}

__m256i
baz (__m128i x)
{
  return _mm256_insertf128_si256 (_mm256_setzero_si256 (), x, 0);
}

__m256i
qux (__m128i x)
{
  return _mm256_insertf128_si256 (_mm256_castsi128_si256 (x), _mm_setzero_si128 (), 1);
}
