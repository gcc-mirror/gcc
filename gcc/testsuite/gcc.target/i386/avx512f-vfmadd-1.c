/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f" } */

#include <x86intrin.h>

void
f1 (__m512d x, __m512d y, __m512d z, __mmask8 m)
{
  register __m512d a __asm ("xmm16"), b __asm ("xmm17"), c __asm ("xmm18");
  a = x; b = y; c = z;
  asm volatile ("" : "+v" (a), "+v" (b), "+v" (c));
  a = _mm512_mask3_fmadd_round_pd (c, b, a, m, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  asm volatile ("" : "+v" (a));
}

void
f2 (__m512 x, __m512 y, __m512 z, __mmask8 m)
{
  register __m512 a __asm ("xmm16"), b __asm ("xmm17"), c __asm ("xmm18");
  a = x; b = y; c = z;
  asm volatile ("" : "+v" (a), "+v" (b), "+v" (c));
  a = _mm512_mask3_fmadd_round_ps (c, b, a, m, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vfmadd231pd\[^\n\r\]*zmm16" } } */
/* { dg-final { scan-assembler "vfmadd231ps\[^\n\r\]*zmm16" } } */
