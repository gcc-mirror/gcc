/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$3\[^\n\r]*\{sae\}\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$7\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$16\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$1\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$2\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$14\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$13\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$20\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$0\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$17\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$18\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$30\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$29\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$4\[^\n\r0-9]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h x, y;
volatile int res;

void extern
avx512f_test (void)
{
  res = _mm_comi_round_sh (x, y, 3, 8);
  res = _mm_comi_sh (x, y, 7);
  res = _mm_comieq_sh (x, y);
  res = _mm_comilt_sh (x, y);
  res = _mm_comile_sh (x, y);
  res = _mm_comigt_sh (x, y);
  res = _mm_comige_sh (x, y);
  res = _mm_comineq_sh (x, y);
  res = _mm_ucomieq_sh (x, y);
  res = _mm_ucomilt_sh (x, y);
  res = _mm_ucomile_sh (x, y);
  res = _mm_ucomigt_sh (x, y);
  res = _mm_ucomige_sh (x, y);
  res = _mm_ucomineq_sh (x, y);
}

