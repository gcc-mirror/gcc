/* { dg-do compile} */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

#include <immintrin.h>
void
__attribute__ ((noinline, noclone))
store512_ph (void *p, __m512h a)
{
  _mm512_store_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)" 1 } } */

void
__attribute__ ((noinline, noclone))
store256_ph (void *p, __m256h a)
{
  _mm256_store_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)" 1 } } */

void
__attribute__ ((noinline, noclone))
store_ph (void *p, __m128h a)
{
  _mm_store_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)" 1 } } */

__m512h
__attribute__ ((noinline, noclone))
load512_ph (void const *p)
{
  return _mm512_load_ph (p);
}

/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)" 1 } } */

__m256h
__attribute__ ((noinline, noclone))
load256_ph (void const *p)
{
  return _mm256_load_ph (p);
}

/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
load_ph (void const *p)
{
  return _mm_load_ph (p);
}
/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)" 1 } } */

__m512h
__attribute__ ((noinline, noclone))
load512u_ph (void const *p)
{
  return _mm512_loadu_ph (p);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^,\]*,\[^\{\n\]*%zmm\[0-9\]" 1 } } */

__m256h
__attribute__ ((noinline, noclone))
load256u_ph (void const *p)
{
  return _mm256_loadu_ph (p);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^,\]*,\[^\{\n\]*%ymm\[0-9\]" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
load128u_ph (void const *p)
{
  return _mm_loadu_ph (p);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^,\]*,\[^\{\n\]*%xmm\[0-9\]" 1 } } */

void
__attribute__ ((noinline, noclone))
store512u_ph (void *p, __m512h a)
{
  return _mm512_storeu_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^\{\n\]*%zmm\[0-9\], *\[^,\]*" 1 } } */

void
__attribute__ ((noinline, noclone))
store256u_ph (void *p, __m256h a)
{
  return _mm256_storeu_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^\{\n\]*%ymm\[0-9\], *\[^,\]*" 1 } } */

void
__attribute__ ((noinline, noclone))
storeu_ph (void *p, __m128h a)
{
  return _mm_storeu_ph (p, a);
}

/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]*\[^\{\n\]*%xmm\[0-9\], *\[^,\]*" 1 } } */

__m512h
__attribute__ ((noinline, noclone))
abs512_ph (__m512h a)
{
  return _mm512_abs_ph (a);
}

/* { dg-final { scan-assembler-times "vpbroadcastd\[^\n\]*%zmm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpandd\[^\n\]*%zmm\[0-9\]+" 1 } } */

__m256h
__attribute__ ((noinline, noclone))
abs256_ph (__m256h a)
{
  return _mm256_abs_ph (a);
}

/* { dg-final { scan-assembler-times "vpbroadcastq\[^\n\]*%ymm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpand\[^\n\]*%ymm\[0-9\]+" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
abs_ph (__m128h a)
{
  return _mm_abs_ph (a);
}

/* { dg-final { scan-assembler-times "vpbroadcastq\[^\n\]*%xmm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpand\[^\n\]*%xmm\[0-9\]+" 1 } } */
