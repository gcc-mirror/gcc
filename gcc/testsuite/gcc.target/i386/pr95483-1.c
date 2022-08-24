/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler-times "pxor\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "pinsrw\[ \\t\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "pextrw\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*(?:\n|\[ \\t\]+#)" 1 } } */


#include <emmintrin.h>
unsigned short *p1,*p2;
volatile __m128i x1,x2; 

void foo (void)
{
   x1=_mm_loadu_si16 (p1);
   _mm_storeu_si16 (p2, x2);
}
