/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -mavx512vl" } */

#include <x86intrin.h>

__attribute__((noinline, noclone)) double
f1 (double x)
{
  register double a __asm__ ("xmm16") = __builtin_round (x);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) float
f2 (float x)
{
  register float a __asm__ ("xmm16") = __builtin_roundf (x);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m128d
f3 (__m128d x, __m128d y)
{
  register __m128d a __asm__ ("xmm16") = x, b __asm__ ("xmm17") = y;
  __asm__ ("" : "+v" (a), "+v" (b));
  a = _mm_round_sd (a, b, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m128
f4 (__m128 x, __m128 y)
{
  register __m128 a __asm__ ("xmm16") = x, b __asm__ ("xmm17") = y;
  __asm__ ("" : "+v" (a), "+v" (b));
  a = _mm_round_ss (a, b, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m128d
f5 (__m128d x)
{
  register __m128d a __asm__ ("xmm16") = x;
  __asm__ ("" : "+v" (a));
  a = _mm_round_pd (a, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m128
f6 (__m128 x)
{
  register __m128 a __asm__ ("xmm16") = x;
  __asm__ ("" : "+v" (a));
  a = _mm_round_ps (a, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m256d
f7 (__m256d x)
{
  register __m256d a __asm__ ("xmm16") = x;
  __asm__ ("" : "+v" (a));
  a = _mm256_round_pd (a, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

__attribute__((noinline, noclone)) __m256
f8 (__m256 x)
{
  register __m256 a __asm__ ("xmm16") = x;
  __asm__ ("" : "+v" (a));
  a = _mm256_round_ps (a, _MM_FROUND_NINT);
  __asm__ ("" : "+v" (a));
  return a;
}

/* Instead of vround{sd,ss,pd,ps} this should use vrndscale{sd,ss,pd,ps}
   counterparts, so that [xy]mm1[67] can be referenced directly in the
   instructions.  */
/* { dg-final { scan-assembler-times "vrndscalesd\[^\n\r\]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "vrndscaless\[^\n\r\]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[^\n\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "vrndscaleps\[^\n\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[^\n\r\]*ymm" 1 } } */
/* { dg-final { scan-assembler-times "vrndscaleps\[^\n\r\]*ymm" 1 } } */
/* { dg-final { scan-assembler-not "vroundsd\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler-not "vroundss\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler-not "vroundpd\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler-not "vroundps\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler-not "vroundpd\[^\n\r\]*ymm" } } */
/* { dg-final { scan-assembler-not "vroundps\[^\n\r\]*ymm" } } */
