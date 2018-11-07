/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpandnd\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %ymm\[0-9\]+, %ymm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[^\n\]*%ymm\[0-9\]+" } } */

#include <immintrin.h>

__m256i
foo (__m256i x, int *f)
{
  return (__m256i) (~(__v8su) x & (__v8su) _mm256_set1_epi32 (*f));
}
