/* PR target/88808  */
/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512dq -O2" } */

#include <immintrin.h>
volatile __mmask8 foo;
void
foo_orb (__m512i a, __m512i b)
{
  __mmask8 m1 = _mm512_cmp_epi64_mask (a, b, 2);
  __mmask8 m2 = _mm512_cmp_epi64_mask (a, b, 4);
  foo = m1 | m2;
}

/* { dg-final { scan-assembler-times "korb\[\t \]" "1" { xfail *-*-* } } }  */

void
foo_xorb (__m512i a, __m512i b)
{
  __mmask8 m1 = _mm512_cmp_epi64_mask (a, b, 2);
  __mmask8 m2 = _mm512_cmp_epi64_mask (a, b, 4);
  foo = m1 ^ m2;
}

/* { dg-final { scan-assembler-times "kxorb\[\t \]" "1" { xfail *-*-* } } }  */

void
foo_andb (__m512i a, __m512i b)
{
  __mmask8 m1 = _mm512_cmp_epi64_mask (a, b, 2);
  __mmask8 m2 = _mm512_cmp_epi64_mask (a, b, 4);
  foo = m1 & m2;
}

void
foo_andnb (__m512i a, __m512i b)
{
  __mmask8 m1 = _mm512_cmp_epi64_mask (a, b, 2);
  __mmask8 m2 = _mm512_cmp_epi64_mask (a, b, 4);
  foo = m1 & ~m2;
}

/* { dg-final { scan-assembler-times "kmovb\[\t \]" "4" { xfail *-*-* } } }  */
