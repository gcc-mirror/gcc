/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

#include <immintrin.h>

__m256h
__attribute__ ((noinline, noclone))
test_mm256_set1_pch (_Float16 _Complex A)
{
  return _mm256_set1_pch(A);
}

__m128h
__attribute__ ((noinline, noclone))
test_mm_set1_pch (_Float16 _Complex A)
{
  return _mm_set1_pch(A);
}

/* { dg-final { scan-assembler-times "vbroadcastss" 2 } } */
