/* { dg-do compile } */
/* { dg-options "-mkl -O2" } */
/* { dg-final { scan-assembler "movl\[ \\t\]+\[^\n\]*ctrl(\\(%rip\\))?\[^\n\r]*%eax" } } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\n\]*k2(\\(%rip\\))?\[^\n\r]*%xmm1" } } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\n\]*k3(\\(%rip\\))?\[^\n\r]*%xmm2" } } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+\[^\n\]*k1(\\(%rip\\))?\[^\n\r]*%xmm0" } } */
/* { dg-final { scan-assembler "loadiwkey\[ \\t\]+\[^\n\]*%xmm1\[^\n\r]*%xmm2" } } */

#include <immintrin.h>

unsigned int ctrl;
__m128i k1, k2, k3;

void
test_keylocker_11 (void)
{
  _mm_loadiwkey (ctrl, k1, k2, k3);
}

