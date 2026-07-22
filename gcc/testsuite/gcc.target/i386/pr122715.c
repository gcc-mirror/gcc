/* PR middle-end/122715 */
/* { dg-do compile } */
/* { dg-options "-O2 -m3dnow -mavx2 -mavx512bw -mavx512vl" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include <immintrin.h>

__m64 avg_u8_3dnow(__m64 x)
{
  return (__m64) __builtin_ia32_pavgusb ((__v8qi) x, (__v8qi) x);
}

__m64 avg_u8_64(__m64 x)
{
  return _mm_avg_pu8 (x, x);
}

__m64 avg_u16_64(__m64 x)
{
  return _mm_avg_pu16 (x, x);
}

__m128i avg_u8_128(__m128i x)
{
  return _mm_avg_epu8 (x, x);
}

__m128i avg_u16_128(__m128i x)
{
  return _mm_avg_epu16 (x, x);
}

__m256i avg_u8_256(__m256i x)
{
  return _mm256_avg_epu8 (x, x);
}

__m256i avg_u16_256(__m256i x)
{
  return _mm256_avg_epu16 (x, x);
}

__m128i avg_u8_128_mask(__m128i x)
{
  return _mm_mask_avg_epu8 (_mm_setzero_si128 (), (__mmask16) -1, x, x);
}

__m128i avg_u16_128_mask(__m128i x)
{
  return _mm_mask_avg_epu16 (_mm_setzero_si128 (), (__mmask8) -1, x, x);
}

__m256i avg_u8_256_mask(__m256i x)
{
  return _mm256_mask_avg_epu8 (_mm256_setzero_si256 (),
			       (__mmask32) -1, x, x);
}

__m256i avg_u16_256_mask(__m256i x)
{
  return _mm256_mask_avg_epu16 (_mm256_setzero_si256 (),
				(__mmask16) -1, x, x);
}

__m512i avg_u8_512(__m512i x)
{
  return _mm512_avg_epu8 (x, x);
}

__m512i avg_u16_512(__m512i x)
{
  return _mm512_avg_epu16 (x, x);
}

/* { dg-final { scan-tree-dump-not {__builtin_ia32_pavg} "optimized" } } */
/* { dg-final { scan-assembler-not {pavg} } } */
