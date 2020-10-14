/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler-times "(?:vpinsrd|movd)\[ \\t\]+\[^\n\]*\\)\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "movd\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 1 } } */

#include <emmintrin.h>
unsigned int *p1,*p2;
volatile __m128i x1,x2; 

void foo (void)
{
   x1=_mm_loadu_si32 (p1);
   _mm_storeu_si32 (p2, x2);
}
