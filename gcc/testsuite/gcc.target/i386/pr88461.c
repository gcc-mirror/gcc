/* PR target/88461 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */
/* { dg-final { scan-assembler-times "kmovw\[ \t]" 2 } } */

#include <x86intrin.h>

int
foo (const __m128i *data, int a)
{
  __m128i v = _mm_load_si128 (data);
  __mmask16 m = _mm_testn_epi16_mask (v, v);
  m = _kshiftli_mask16 (m, 1);
  m = _kandn_mask16 (m, a);
  return m;
}
