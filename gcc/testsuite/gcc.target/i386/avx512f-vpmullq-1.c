/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512dq -O2" } */
/* { dg-final { scan-assembler-times "vpmullq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  0 } } */
/* { dg-final { scan-assembler-times "vpmullq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 0 } } */
/* { dg-final { scan-assembler-times "vpmuludq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  6 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \\t\]+\[^\{\n\]*\\\$32\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  4 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \\t\]+\[^\{\n\]*\\\$32\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vpaddq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  3 } } */
/* { dg-final { scan-assembler-times "vpaddq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i _x1, _y1, _z1;

void extern
avx512f_test (void)
{
  _x1 = _mm512_mullox_epi64 (_y1, _z1);
  _x1 = _mm512_mask_mullox_epi64 (_x1, 3, _y1, _z1);
}
