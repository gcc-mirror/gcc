/* PR target/101472 */
/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpscatterqd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdd\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterqq\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpscatterdq\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqps\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdps\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqpd\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterdpd\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

void two_scatters_epi32(void* addr, __mmask8 k1, __mmask8 k2, __m512i vindex, 
			 __m256i a, __m512i b)
{
    _mm512_mask_i64scatter_epi32(addr, k1, vindex, a, 1);
    _mm512_mask_i64scatter_epi32(addr, k2, vindex, a, 1);
    _mm512_mask_i32scatter_epi32(addr, k1, vindex, b, 1);
    _mm512_mask_i32scatter_epi32(addr, k2, vindex, b, 1);
}

void two_scatters_epi64(void* addr, __mmask8 k1, __mmask8 k2, __m512i vindex, 
			 __m256i idx, __m512i a)
{
    _mm512_mask_i64scatter_epi64(addr, k1, vindex, a, 1);
    _mm512_mask_i64scatter_epi64(addr, k2, vindex, a, 1);
    _mm512_mask_i32scatter_epi64(addr, k1, idx, a, 1);
    _mm512_mask_i32scatter_epi64(addr, k2, idx, a, 1);
}

void two_scatters_ps(void* addr, __mmask8 k1, __mmask8 k2, __m512i vindex, 
		      __m256 a, __m512 b)
{
    _mm512_mask_i64scatter_ps(addr, k1, vindex, a, 1);
    _mm512_mask_i64scatter_ps(addr, k2, vindex, a, 1);
    _mm512_mask_i32scatter_ps(addr, k1, vindex, b, 1);
    _mm512_mask_i32scatter_ps(addr, k2, vindex, b, 1);
}

void two_scatters_pd(void* addr, __mmask8 k1, __mmask8 k2, __m512i vindex, 
		      __m256i idx, __m512d a)
{
    _mm512_mask_i64scatter_pd(addr, k1, vindex, a, 1);
    _mm512_mask_i64scatter_pd(addr, k2, vindex, a, 1);
    _mm512_mask_i32scatter_pd(addr, k1, idx, a, 1);
    _mm512_mask_i32scatter_pd(addr, k2, idx, a, 1);
}
