/* { dg-do compile } */

#include <immintrin.h>

#pragma GCC target("avx512vl,avx512dq")

void foo (__m256i i)
{
  volatile __m256d v1 = _mm256_cvtepi64_pd (i);
}

