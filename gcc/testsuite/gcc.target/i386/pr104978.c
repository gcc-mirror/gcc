/* PR target/104978 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}+(?:\n|\[ \\t\]+#)" 2 } } */

#include<immintrin.h>

__m128h
foo (__m128h a, __m128h b, __m128h c, __mmask8 m)
{ 
  return _mm_mask_fmadd_round_sch (a, m, b, c, 8);
}

__m128h
foo2 (__m128h a, __m128h b, __m128h c, __mmask8 m)
{ 
  return _mm_mask_fcmadd_round_sch (a, m, b, c, 8);
}
