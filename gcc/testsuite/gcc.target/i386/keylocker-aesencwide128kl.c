/* { dg-do compile } */
/* { dg-options "-mwidekl -O2" } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata(\\(%rip\\))?\[^\n\r]*%xmm0" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+16(\\(%rip\\))?\[^\n\r]*%xmm1" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+32(\\(%rip\\))?\[^\n\r]*%xmm2" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+48(\\(%rip\\))?\[^\n\r]*%xmm3" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+64(\\(%rip\\))?\[^\n\r]*%xmm4" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+80(\\(%rip\\))?\[^\n\r]*%xmm5" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+96(\\(%rip\\))?\[^\n\r]*%xmm6" } } */
/* { dg-final { scan-assembler "movdqu\[ \\t\]+\[^\n\]*idata\\+112(\\(%rip\\))?\[^\n\r]*%xmm7" } } */
/* { dg-final { scan-assembler "aesencwide128kl\[ \\t\]+\[^\n\]*h1(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "sete" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm0\[^\n\r]*odata(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm1\[^\n\r]*odata\\+16(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm2\[^\n\r]*odata\\+32(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm3\[^\n\r]*odata\\+48(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm4\[^\n\r]*odata\\+64(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm5\[^\n\r]*odata\\+80(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm6\[^\n\r]*odata\\+96(\\(%rip\\))?" } } */
/* { dg-final { scan-assembler "(?:movdqu|movups)\[ \\t\]+\[^\n\]*%xmm7\[^\n\r]*odata\\+112(\\(%rip\\))?" } } */

#include <immintrin.h>

const char h1[48];
const __m128i idata[8];
__m128i odata[8];

unsigned char
test_keylocker_7 (void)
{
  return _mm_aesencwide128kl_u8 (odata, idata, h1);
}
