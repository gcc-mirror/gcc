/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

#include<immintrin.h>
extern __m256i res;
void
foo (__m512 a, __mmask16 m)
{
  res = _mm512_maskz_cvtps_ph (m, a, 10);
}

/* { dg-final { scan-assembler-not "vcvtps2ph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]\[^\n\]*res\[^\n\]*\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"} } */
