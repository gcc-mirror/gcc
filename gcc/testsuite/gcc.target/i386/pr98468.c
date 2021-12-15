/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-not "vxorp" } } */
/* { dg-final { scan-assembler-not "vandnp" } } */

#include<immintrin.h>
__m128 f(__m128 val)
{
    return _mm_andnot_ps(_mm_set_ps1(0.0f), val);
}

__m256 f2(__m256 val)
{
    return _mm256_andnot_ps(_mm256_set1_ps(0.0f), val);
}

__m512 f3(__m512 val)
{
    return _mm512_andnot_ps(_mm512_set1_ps(0.0f), val);
}

__m128d f4(__m128d val)
{
    return _mm_andnot_pd(_mm_set_pd1(0.0), val);
}

__m256d f5(__m256d val)
{
    return _mm256_andnot_pd(_mm256_set1_pd(0.0), val);
}

__m512d f6(__m512d val)
{
    return _mm512_andnot_pd(_mm512_set1_pd(0.0), val);
}
