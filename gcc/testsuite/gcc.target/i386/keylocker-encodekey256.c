/* { dg-do compile } */
/* { dg-options "-mkl -O2" } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\n\]*k1(\\(%rip\\))?\[^\n\r]*%xmm0" } } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\n\]*k2(\\(%rip\\))?\[^\n\r]*%xmm1" } } */
/* { dg-final { scan-assembler "movl\[ \\t\]+\[^\n\]*ctrl(\\(%rip\\))?\[^\n\r]*%eax" } } */
/* { dg-final { scan-assembler "encodekey256\[ \\t\]+\[^\n\]*%eax\[^\n\r]*%eax" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm0\[^\n\r]*h2(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm1\[^\n\r]*h2\\+16(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm2\[^\n\r]*h2\\+32(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm3\[^\n\r]*h2\\+48(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqa|movaps)\[ \\t\]+\[^\n\]*%xmm\[4-6\]\[^\n\r]*k3(\\(%rip\\))?" } } */

#include <immintrin.h>

unsigned int ctrl;
char h2[48];
__m128i k1, k2, k3;

unsigned int
test_keylocker_10 (void)
{
  unsigned int ret;
  ret = _mm_encodekey256_u32 (ctrl, k1, k2, h2);

  if (ret)
    k3 = (__m128i){0};

  return ret;
}

