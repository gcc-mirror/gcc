/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavxvnni -mavx512vnni -mavx512vl" } */
/* { dg-final { scan-assembler-times "\\tvpdpbusd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpbusd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpbusds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpbusds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpwssd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpwssd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpwssds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "\\tvpdpwssds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m256i x,y,z;
volatile __m128i x_,y_,z_;

void
avxvnni_test (void)
{
  register __m256i a __asm ("xmm16");
  register __m128i a_ __asm ("xmm26");
  a = _mm256_dpbusd_epi32 (x, y, z);
  asm volatile ("" : "+v" (a));
  x = a;
  a_ = _mm_dpbusd_epi32 (x_, y_, z_);
  asm volatile ("" : "+v" (a_));
  x_ = a_;
  a = _mm256_dpbusds_epi32 (x, y, z);
  asm volatile ("" : "+v" (a));
  x = a;
  a_ = _mm_dpbusds_epi32 (x_, y_, z_);
  asm volatile ("" : "+v" (a_));
  x_ = a_;
  a = _mm256_dpwssd_epi32 (x, y, z);
  asm volatile ("" : "+v" (a));
  x = a;
  a_ = _mm_dpwssd_epi32 (x_, y_, z_);
  asm volatile ("" : "+v" (a_));
  x_ = a_;
  a = _mm256_dpwssds_epi32 (x, y, z);
  asm volatile ("" : "+v" (a));
  x = a;
  a_ = _mm_dpwssds_epi32 (x_, y_, z_);
  asm volatile ("" : "+v" (a_));
  x_ = a_;
}
