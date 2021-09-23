/* { dg-do compile } */
/* { dg-options "-mkl -O2" } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\\n\\r\]*, %xmm0" } } */
/* { dg-final { scan-assembler "aesenc256kl\[ \\t\]+\[^\\n\\r\]*, %xmm0" } } */
/* { dg-final { scan-assembler "j\[ez\]" } } */
/* { dg-final { scan-assembler "sete" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm0,\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm0, %xmm0" } } */

#include <immintrin.h>

__m128i k1, k2;
const char h1[48];

unsigned char
test_keylocker_4 (void)
{
  return _mm_aesenc256kl_u8 (&k1, k2, h1);
}
