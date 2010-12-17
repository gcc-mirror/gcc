/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-options "-O2 -msse2 -mno-vect8-ret-in-mem" { target i?86-*-solaris2.[89] *-*-vxworks* } } */

#include <mmintrin.h>

__m64
unsigned_add3 (const __m64 * a, const __m64 * b, unsigned int count)
{
  __m64 sum;
  unsigned int i;

  for (i = 1; i < count; i++)
    sum = _mm_add_si64 (a[i], b[i]);

  return sum;
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+\[^\n\]*%mm" 1 } } */
