/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-not {(?n)vfmadd[1-3]*ps.*\(} { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {(?n)vfmadd[1-3]*ps[ \t]*} 3 } } */

#include<immintrin.h>

void
foo (__m512 pv, __m512 a, __m512 b, __m512 c,
     __m512* pdest, __m512* p1)
{
  __m512 t = *p1;
    pdest[0] = _mm512_fmadd_ps (t, pv, a);
    pdest[1] = _mm512_fmadd_ps (t, pv, b);
    pdest[2] = _mm512_fmadd_ps (t, pv, c);
}
