/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */
#include "math_m_pi.h"
#define AVX512F
#include "avx512f-helper.h"

void
TEST (void)
{
  __m512 a = _mm512_set1_ps ((float) M_PI);
  __m512 b = _mm512_set1_ps ((float) 1.f);

  __m512 result1 = _mm512_add_round_ps (a, b, (_MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC));
  __m512 result2 = _mm512_add_round_ps (a, b, (_MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC));
   
  if (result1[0] == result2[0])
    abort ();
}
