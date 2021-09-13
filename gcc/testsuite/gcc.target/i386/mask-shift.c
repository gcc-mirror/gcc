/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512dq -O2" } */

#include<immintrin.h>
void
fooq (__m512i a, __m512i b, void* p)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  m1 >>= 4;
  _mm512_mask_storeu_epi64 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftrb} "1" } }  */

void
food (__m512i a, __m512i b, void* p)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  m1 >>= 8;
  _mm512_mask_storeu_epi32 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftrw} "1" } }  */

void
foow (__m512i a, __m512i b, void* p)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  m1 >>= 16;
  _mm512_mask_storeu_epi16 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftrd} "1" } }  */

void
foob (__m512i a, __m512i b, void* p)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  m1 >>= 32;
  _mm512_mask_storeu_epi8 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftrq} "1" { target { ! ia32 } } } }  */

void
fooq1 (__m512i a, __m512i b, void* p)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  m1 <<= 4;
  _mm512_mask_storeu_epi64 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftlb} "1" } }  */

void
food1 (__m512i a, __m512i b, void* p)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  m1 <<= 8;
  _mm512_mask_storeu_epi32 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftlw} "1" } }  */

void
foow1 (__m512i a, __m512i b, void* p)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  m1 <<= 16;
  _mm512_mask_storeu_epi16 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftld} "1" } }  */

void
foob1 (__m512i a, __m512i b, void* p)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  m1 <<= 32;
  _mm512_mask_storeu_epi8 (p, m1, a);
}

/* { dg-final { scan-assembler-times {(?n)kshiftlq} "1" { target { ! ia32 } } } }  */
