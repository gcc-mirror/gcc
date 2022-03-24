/* PR target/104977 */
/* { dg-do assemble } */
/* { dg-options "-O2 -mavx512fp16 -masm=intel" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-require-effective-target masm_intel } */

#include<immintrin.h>

__m128h
foo (__m128h a, __m128h b, __m128h c, __mmask8 m)
{
  return _mm_fcmadd_round_sch (a, b, c, 8);
}
