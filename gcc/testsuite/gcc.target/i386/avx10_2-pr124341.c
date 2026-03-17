/* PR target/124341 */
/* { dg-do assemble { target { avx10_2 && masm_intel } } } */
/* { dg-options "-O2 -mavx10.2 -masm=intel" } */

#include <x86intrin.h>

__m128i
mm_mask_cvt2ph_bf8 (__m128i w, __mmask16 u, __m128h a, __m128h b)
{
  return _mm_mask_cvt2ph_bf8 (w, u, a, b);
}

__m256i
mm256_mask_cvt2ph_bf8 (__m256i w, __mmask32 u, __m256h a, __m256h b)
{
  return _mm256_mask_cvt2ph_bf8 (w, u, a, b);
}

__m512i
mm512_mask_cvt2ph_bf8 (__m512i w, __mmask64 u, __m512h a, __m512h b)
{
  return _mm512_mask_cvt2ph_bf8 (w, u, a, b);
}

__m128i
mm_mask_cvts_2ph_bf8 (__m128i w, __mmask16 u, __m128h a, __m128h b)
{
  return _mm_mask_cvts_2ph_bf8 (w, u, a, b);
}

__m256i
mm256_mask_cvts_2ph_bf8 (__m256i w, __mmask32 u, __m256h a, __m256h b)
{
  return _mm256_mask_cvts_2ph_bf8 (w, u, a, b);
}

__m512i
mm512_mask_cvts_2ph_bf8 (__m512i w, __mmask64 u, __m512h a, __m512h b)
{
  return _mm512_mask_cvts_2ph_bf8 (w, u, a, b);
}

__m128i
mm_mask_cvt2ph_hf8 (__m128i w, __mmask16 u, __m128h a, __m128h b)
{
  return _mm_mask_cvt2ph_hf8 (w, u, a, b);
}

__m256i
mm256_mask_cvt2ph_hf8 (__m256i w, __mmask32 u, __m256h a, __m256h b)
{
  return _mm256_mask_cvt2ph_hf8 (w, u, a, b);
}

__m512i
mm512_mask_cvt2ph_hf8 (__m512i w, __mmask64 u, __m512h a, __m512h b)
{
  return _mm512_mask_cvt2ph_hf8 (w, u, a, b);
}

__m128i
mm_mask_cvts_2ph_hf8 (__m128i w, __mmask16 u, __m128h a, __m128h b)
{
  return _mm_mask_cvts_2ph_hf8 (w, u, a, b);
}

__m256i
mm256_mask_cvts_2ph_hf8 (__m256i w, __mmask32 u, __m256h a, __m256h b)
{
  return _mm256_mask_cvts_2ph_hf8 (w, u, a, b);
}

__m512i
mm512_mask_cvts_2ph_hf8 (__m512i w, __mmask64 u, __m512h a, __m512h b)
{
  return _mm512_mask_cvts_2ph_hf8 (w, u, a, b);
}
