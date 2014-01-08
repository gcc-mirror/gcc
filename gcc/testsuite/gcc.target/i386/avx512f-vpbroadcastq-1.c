/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 2  } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+%r\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+%r\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+%r\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 { target { ! { ia32 } } } } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m128i y;
volatile long long z;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_broadcastq_epi64 (y);
  x = _mm512_mask_broadcastq_epi64 (x, m, y);
  x = _mm512_maskz_broadcastq_epi64 (m, y);

  x = _mm512_set1_epi64 (z);
  x = _mm512_mask_set1_epi64 (x, m, z);
  x = _mm512_maskz_set1_epi64 (m, z);
}
