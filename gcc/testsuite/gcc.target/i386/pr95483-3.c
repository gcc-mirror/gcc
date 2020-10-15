/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-times "vmovd\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __m256i x1; 

int foo (void)
{
   return _mm256_cvtsi256_si32 (x1);
}
