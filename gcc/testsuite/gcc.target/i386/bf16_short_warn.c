/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include<immintrin.h>
typedef struct {
short payload;
} BFloat16;

__attribute__((target("avx512vl,avx512bf16")))
BFloat16 tobf16_avx512(float f)
{
    BFloat16 r;
    __m128bh m = _mm_cvtneps_pbh(_mm_set_ss(f));
    r.payload = m[0]; /* { dg-warning " be careful of implicit conversion between '__bf16' and 'short'" } */
    return r;
}

