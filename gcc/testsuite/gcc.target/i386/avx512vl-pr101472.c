/* PR target/101472 */
/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpscatterqd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterqd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterqq\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterqq\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdq\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdq\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqps\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqps\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdps\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdps\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqpd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqpd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdpd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdpd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */


#include <immintrin.h>

void two_scatters_epi32(void* addr, __mmask8 k1, __mmask8 k2, __m128i vindex1, 
			 __m256i vindex2, __m128i src_epi32, 
		         __m256i src_i32_epi32)
{
    _mm_mask_i64scatter_epi32(addr, k1, vindex1, src_epi32, 1);
    _mm_mask_i64scatter_epi32(addr, k2, vindex1, src_epi32, 1);
    _mm256_mask_i64scatter_epi32(addr, k1, vindex2, src_epi32, 1);
    _mm256_mask_i64scatter_epi32(addr, k2, vindex2, src_epi32, 1);

    _mm_mask_i32scatter_epi32(addr, k1, vindex1, src_epi32, 1);
    _mm_mask_i32scatter_epi32(addr, k2, vindex1, src_epi32, 1);
    _mm256_mask_i32scatter_epi32(addr, k1, vindex2, src_i32_epi32, 1);
    _mm256_mask_i32scatter_epi32(addr, k2, vindex2, src_i32_epi32, 1);
}

void two_scatters_epi64(void* addr, __mmask8 k1, __mmask8 k2, __m128i vindex1, 
		         __m256i vindex2, __m128i src_epi64_mm, 
			 __m256i src_epi64)
{
    _mm_mask_i64scatter_epi64(addr, k1, vindex1, src_epi64_mm, 1);
    _mm_mask_i64scatter_epi64(addr, k2, vindex1, src_epi64_mm, 1);
    _mm256_mask_i64scatter_epi64(addr, k1, vindex2, src_epi64, 1);
    _mm256_mask_i64scatter_epi64(addr, k2, vindex2, src_epi64, 1);

    _mm_mask_i32scatter_epi64(addr, k1, vindex1, src_epi64_mm, 8);
    _mm_mask_i32scatter_epi64(addr, k2, vindex1, src_epi64_mm, 8);
    _mm256_mask_i32scatter_epi64(addr, k1, vindex1, src_epi64, 1);
    _mm256_mask_i32scatter_epi64(addr, k2, vindex1, src_epi64, 1);
}
void two_scatters_ps(void* addr, __mmask8 k1, __mmask8 k2, __m128i vindex1, 
		      __m256i vindex2, __m128 src_ps, __m256 src_i32_ps)
{
    _mm_mask_i64scatter_ps(addr, k1, vindex1, src_ps, 1);
    _mm_mask_i64scatter_ps(addr, k2, vindex1, src_ps, 1);
    _mm256_mask_i64scatter_ps(addr, k1, vindex2, src_ps, 1);
    _mm256_mask_i64scatter_ps(addr, k2, vindex2, src_ps, 1);

    _mm_mask_i32scatter_ps(addr, k1, vindex1, src_ps, 8);
    _mm_mask_i32scatter_ps(addr, k2, vindex1, src_ps, 8);
    _mm256_mask_i32scatter_ps(addr, k1, vindex2, src_i32_ps, 1);
    _mm256_mask_i32scatter_ps(addr, k2, vindex2, src_i32_ps, 1);
}

void two_scatters_pd(void* addr, __mmask8 k1, __mmask8 k2,  __m128i vindex1,
		      __m256i vindex2, __m128d src_pd_mm, __m256d src_pd)
{
    _mm_mask_i64scatter_pd(addr, k1, vindex1, src_pd_mm, 1);
    _mm_mask_i64scatter_pd(addr, k2, vindex1, src_pd_mm, 1);
    _mm256_mask_i64scatter_pd(addr, k1, vindex2, src_pd, 1);
    _mm256_mask_i64scatter_pd(addr, k2, vindex2, src_pd, 1);

    _mm_mask_i32scatter_pd(addr, k1, vindex1, src_pd_mm, 8);
    _mm_mask_i32scatter_pd(addr, k2, vindex1, src_pd_mm, 8);
    _mm256_mask_i32scatter_pd(addr, k1, vindex1, src_pd, 1);
    _mm256_mask_i32scatter_pd(addr, k2, vindex1, src_pd, 1);
}
