/* { dg-do compile } */
/* { dg-options "-mwidekl -O2" } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*, %xmm0" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*16\[^\\n\\r\]*, %xmm1" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*32\[^\\n\\r\]*, %xmm2" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*48\[^\\n\\r\]*, %xmm3" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*64\[^\\n\\r\]*, %xmm4" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*80\[^\\n\\r\]*, %xmm5" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*96\[^\\n\\r\]*, %xmm6" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\\n\\r\]*112\[^\\n\\r\]*, %xmm7" } } */
/* { dg-final { scan-assembler "aesencwide256kl\[ \\t\]+\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "j\[ez\]" } } */
/* { dg-final { scan-assembler "sete" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm0,\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm1,\[^\\n\\r\]*16\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm2,\[^\\n\\r\]*32\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm3,\[^\\n\\r\]*48\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm4,\[^\\n\\r\]*64\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm5,\[^\\n\\r\]*80\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm6,\[^\\n\\r\]*96\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\\n\\r\]*%xmm7,\[^\\n\\r\]*112\[^\\n\\r\]*" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm7, %xmm7" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm0" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm1" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm2" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm3" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm4" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm5" } } */
/* { dg-final { scan-assembler "movdqa\[ \t\]+%xmm7, %xmm6" } } */

#include <immintrin.h>

const char h1[48];
const __m128i idata[8];
__m128i odata[8];

unsigned char
test_keylocker_8 (void)
{
  return _mm_aesencwide256kl_u8 (odata, idata, h1);
}

