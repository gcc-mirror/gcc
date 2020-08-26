/* PR target/96744  */
/* { dg-do compile } */
/* { dg-options "-mavx512vp2intersect -O2" } */

#include<immintrin.h>
void
_mm512_2intersect_epi64_cut (__m512i __A, __m512i __B, __mmask8 *__U,
    __mmask8 *__M)
{
  __builtin_ia32_2intersectq512 (__U, __M, (__v8di) __A, (__v8di) __B);
}

void
_mm512_2intersect_epi32_cut (__m512i __A, __m512i __B, __mmask16 *__U,
    __mmask16 *__M)
{
  __builtin_ia32_2intersectd512 (__U, __M, (__v16si) __A, (__v16si) __B);
}

