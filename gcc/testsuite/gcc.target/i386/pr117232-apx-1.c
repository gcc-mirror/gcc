/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512bw -mavx512vl -mavx512dq -mapxf -O2" } */
/* { dg-final { scan-assembler-times {(?n)kortest[bwqd]} 7 } } */
/* { dg-final { scan-assembler-times {(?n)cmov([lq]\.)?n?c} 7 } } */

#include <immintrin.h>

int
foo (__m512i a, __m512i b, int c, int d) {
  __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
  return k == (__mmask64) -1 ? c : d;
}

int
foo1 (__m512i a, __m512i b, int c, int d) {
  __mmask32 k = _mm512_cmpeq_epi16_mask (a, b);
  return k == (__mmask32) -1 ? c : d;
}

int
foo2 (__m512i a, __m512i b, int c, int d) {
  __mmask16 k = _mm512_cmpeq_epi32_mask (a, b);
  return k == (__mmask16) -1 ? c : d;
}

int
foo3 (__m512i a, __m512i b, int c, int d) {
  __mmask8 k = _mm512_cmpeq_epi64_mask (a, b);
  return k == (__mmask8) -1 ? c : d;
}

short
foo4 (__m512i a, __m512i b, short c, short d) {
  __mmask8 k = _mm512_cmpeq_epi64_mask (a, b);
  return k == (__mmask8) -1 ? c : d;
}

char
foo5 (__m512i a, __m512i b, char c, char d) {
  __mmask64 k = _mm512_cmpeq_epi8_mask (a, b);
  return k == (__mmask64) -1 ? c : d;
}

long long
foo6 (__m512i a, __m512i b, long long c, long long d) {
  __mmask16 k = _mm512_cmpeq_epi32_mask (a, b);
  return k == (__mmask16) -1 ? c : d;
}
