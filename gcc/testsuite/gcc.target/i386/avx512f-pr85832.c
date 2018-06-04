/* PR target/85832 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512vl -mno-avx512bw -masm=att" } */
/* { dg-final { scan-assembler-times {\mvptestnmd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvptestnmq\M} 1 } } */

#include <x86intrin.h>

int
f1 (__m512i x)
{
  return _mm512_cmpeq_epi32_mask (x, _mm512_setzero_si512 ());
}

int
f2 (__m512i x)
{
  return _mm512_cmpeq_epi64_mask (x, _mm512_setzero_si512 ());
}
