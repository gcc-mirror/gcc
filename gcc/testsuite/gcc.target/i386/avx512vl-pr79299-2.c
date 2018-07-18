/* PR target/79299 */
/* { dg-do assemble { target avx512vl } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-Ofast -mavx512vl -masm=intel" } */

#include <immintrin.h>

__m512
f1 (__m512i x, void const *y)
{
  return _mm512_i32gather_ps (x, y, 1);
}

__m512
f2 (__m512 x, __mmask16 y, __m512i z, void const *w)
{
  return _mm512_mask_i32gather_ps (x, y, z, w, 1);
}

__m512d
f3 (__m256i x, void const *y)
{
  return _mm512_i32gather_pd (x, y, 1);
}

__m512d
f4 (__m512d x, __mmask8 y, __m256i z, void const *w)
{
  return _mm512_mask_i32gather_pd (x, y, z, w, 1);
}

__m256
f5 (__m512i x, void const *y)
{
  return _mm512_i64gather_ps (x, y, 1);
}

__m256
f6 (__m256 x, __mmask16 y, __m512i z, void const *w)
{
  return _mm512_mask_i64gather_ps (x, y, z, w, 1);
}

__m512d
f7 (__m512i x, void const *y)
{
  return _mm512_i64gather_pd (x, y, 1);
}

__m512d
f8 (__m512d x, __mmask8 y, __m512i z, void const *w)
{
  return _mm512_mask_i64gather_pd (x, y, z, w, 1);
}

__m512i
f9 (__m512i x, void const *y)
{
  return _mm512_i32gather_epi32 (x, y, 1);
}

__m512i
f10 (__m512i x, __mmask16 y, __m512i z, void const *w)
{
  return _mm512_mask_i32gather_epi32 (x, y, z, w, 1);
}

__m512i
f11 (__m256i x, void const *y)
{
  return _mm512_i32gather_epi64 (x, y, 1);
}

__m512i
f12 (__m512i x, __mmask8 y, __m256i z, void const *w)
{
  return _mm512_mask_i32gather_epi64 (x, y, z, w, 1);
}

__m256i
f13 (__m512i x, void const *y)
{
  return _mm512_i64gather_epi32 (x, y, 1);
}

__m256i
f14 (__m256i x, __mmask16 y, __m512i z, void const *w)
{
  return _mm512_mask_i64gather_epi32 (x, y, z, w, 1);
}

__m512i
f15 (__m512i x, void const *y)
{
  return _mm512_i64gather_epi64 (x, y, 1);
}

__m512i
f16 (__m512i x, __mmask8 y, __m512i z, void const *w)
{
  return _mm512_mask_i64gather_epi64 (x, y, z, w, 1);
}

__m256
f17 (__m256 x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i32gather_ps (x, y, z, w, 1);
}

__m128
f18 (__m128 x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_ps (x, y, z, w, 1);
}

__m256d
f19 (__m256d x, __mmask8 y, __m128i z, void const *w)
{
  return _mm256_mmask_i32gather_pd (x, y, z, w, 1);
}

__m128d
f20 (__m128d x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_pd (x, y, z, w, 1);
}

__m128
f21 (__m128 x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_ps (x, y, z, w, 1);
}

__m128
f22 (__m128 x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_ps (x, y, z, w, 1);
}

__m256d
f23 (__m256d x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_pd (x, y, z, w, 1);
}

__m128d
f24 (__m128d x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_pd (x, y, z, w, 1);
}

__m256i
f25 (__m256i x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i32gather_epi32 (x, y, z, w, 1);
}

__m128i
f26 (__m128i x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_epi32 (x, y, z, w, 1);
}

__m256i
f27 (__m256i x, __mmask8 y, __m128i z, void const *w)
{
  return _mm256_mmask_i32gather_epi64 (x, y, z, w, 1);
}

__m128i
f28 (__m128i x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_epi64 (x, y, z, w, 1);
}

__m128i
f29 (__m128i x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_epi32 (x, y, z, w, 1);
}

__m128i
f30 (__m128i x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_epi32 (x, y, z, w, 1);
}

__m256i
f31 (__m256i x, __mmask8 y, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_epi64 (x, y, z, w, 1);
}

__m128i
f32 (__m128i x, __mmask8 y, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_epi64 (x, y, z, w, 1);
}

__m256
f33 (__m256 x, __m256i z, void const *w)
{
  return _mm256_mmask_i32gather_ps (x, -1, z, w, 1);
}

__m128
f34 (__m128 x, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_ps (x, -1, z, w, 1);
}

__m256d
f35 (__m256d x, __m128i z, void const *w)
{
  return _mm256_mmask_i32gather_pd (x, -1, z, w, 1);
}

__m128d
f36 (__m128d x, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_pd (x, -1, z, w, 1);
}

__m128
f37 (__m128 x, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_ps (x, -1, z, w, 1);
}

__m128
f38 (__m128 x, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_ps (x, -1, z, w, 1);
}

__m256d
f39 (__m256d x, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_pd (x, -1, z, w, 1);
}

__m128d
f40 (__m128d x, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_pd (x, -1, z, w, 1);
}

__m256i
f41 (__m256i x, __m256i z, void const *w)
{
  return _mm256_mmask_i32gather_epi32 (x, -1, z, w, 1);
}

__m128i
f42 (__m128i x, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_epi32 (x, -1, z, w, 1);
}

__m256i
f43 (__m256i x, __m128i z, void const *w)
{
  return _mm256_mmask_i32gather_epi64 (x, -1, z, w, 1);
}

__m128i
f44 (__m128i x, __m128i z, void const *w)
{
  return _mm_mmask_i32gather_epi64 (x, -1, z, w, 1);
}

__m128i
f45 (__m128i x, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_epi32 (x, -1, z, w, 1);
}

__m128i
f46 (__m128i x, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_epi32 (x, -1, z, w, 1);
}

__m256i
f47 (__m256i x, __m256i z, void const *w)
{
  return _mm256_mmask_i64gather_epi64 (x, -1, z, w, 1);
}

__m128i
f48 (__m128i x, __m128i z, void const *w)
{
  return _mm_mmask_i64gather_epi64 (x, -1, z, w, 1);
}
