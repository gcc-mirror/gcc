/* PR target/85832 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mno-avx512vl -masm=att" } */
/* { dg-final { scan-assembler-times {\mvptestnmb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvptestnmw\M} 1 } } */

#include <x86intrin.h>

int
f1 (__m512i x)
{
  return _mm512_cmpeq_epi8_mask (x, _mm512_setzero_si512 ());
}

int
f2 (__m512i x)
{
  return _mm512_cmpeq_epi16_mask (x, _mm512_setzero_si512 ());
}
