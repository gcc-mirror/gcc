/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vmovss" } } */

#include <immintrin.h>

__m128
foo (float *x)
{
  return _mm_broadcastss_ps(_mm_load_ss(x));
}
