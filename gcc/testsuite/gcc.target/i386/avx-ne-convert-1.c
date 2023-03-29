/* { dg-do compile } */
/* { dg-options "-mavxneconvert -O2" } */
/* { dg-final { scan-assembler-times "vbcstnebf162ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbcstnebf162ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbcstnesh2ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbcstnesh2ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneebf162ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneebf162ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneeph2ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneeph2ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneobf162ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneobf162ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneoph2ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneoph2ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vcvtneps2bf16x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vcvtneps2bf16y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
#include <immintrin.h>

volatile __m128 x1;
volatile __m256 x2;
volatile __m128bh res1, res2;
const void *a;
__m128bh *b;
__m256bh *c;
__m128h *d;
__m256h *e;

void extern
avx_ne_convert_test (void)
{
  x1 = _mm_bcstnebf16_ps (a);
  x2 = _mm256_bcstnebf16_ps (a);
  x1 = _mm_bcstnesh_ps (a);
  x2 = _mm256_bcstnesh_ps (a);
  x1 = _mm_cvtneebf16_ps (b);
  x2 = _mm256_cvtneebf16_ps (c);
  x1 = _mm_cvtneeph_ps (d);
  x2 = _mm256_cvtneeph_ps (e);
  x1 = _mm_cvtneobf16_ps (b);
  x2 = _mm256_cvtneobf16_ps (c);
  x1 = _mm_cvtneoph_ps (d);
  x2 = _mm256_cvtneoph_ps (e);
  res1 = _mm_cvtneps_avx_pbh (x1);
  res2 = _mm256_cvtneps_avx_pbh (x2);
}
