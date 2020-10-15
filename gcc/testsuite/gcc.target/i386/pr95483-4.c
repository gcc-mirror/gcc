/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vmovd\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __m512i x1; 

int foo (void)
{
   return _mm512_cvtsi512_si32 (x1);
}
