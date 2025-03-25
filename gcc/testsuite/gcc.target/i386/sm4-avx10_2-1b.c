/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -msm4 -mavx10.2" } */
/* { dg-final { scan-assembler "vsm4key4\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm4rnds4\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

volatile __m512i x, y, z;

void extern
sm4_test (void)
{
  x = _mm512_sm4key4_epi32 (y, z);
  x = _mm512_sm4rnds4_epi32 (y, z);
}
