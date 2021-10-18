/* { dg-do compile } */
/* { dg-options "-O2" } */

#include<immintrin.h>
typedef float __m256 __attribute__((__vector_size__(32)));
__m256 _mm256_blendv_ps___Y, _mm256_blendv_ps___M, _mm256_mul_ps___A,
  _mm256_mul_ps___B, IfThenElse___trans_tmp_9;

void
__attribute__ ((target("avx")))
IfThenElse (__m256 no) {
  IfThenElse___trans_tmp_9 = _mm256_blendv_ps (no, _mm256_blendv_ps___Y, _mm256_blendv_ps___M);
}
void
__attribute__ ((target("avx512vl")))
EncodedFromDisplay() {
  __m256 __trans_tmp_11 = _mm256_mul_ps___A * _mm256_mul_ps___B;
  IfThenElse(__trans_tmp_11);
}
