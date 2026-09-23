/* PR target/126581  */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -march=x86-64-v4 -mavx10.2 -Wno-deprecated-declarations" } */
/* { dg-final { scan-assembler-times "vcvttss2usis\[ \t]*%xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttss2usis\[ \t]*\{sae\}, %xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttss2sis\[ \t]*%xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttss2sis\[ \t]*\{sae\}, %xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsd2usis\[ \t]*%xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsd2usis\[ \t]*\{sae\}, %xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsd2sis\[ \t]*%xmm0, %eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsd2sis\[ \t]*\{sae\}, %xmm0, %eax" 1 } } */

#include <x86intrin.h>

unsigned long long
func1 (__m128 x)
{
  return _mm_cvtts_ss_epu32 (x);
}

unsigned long long
func2 (__m128 x)
{
  return _mm_cvtts_roundss_epu32
    (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}

unsigned long long
func3 (__m128 x)
{
  return (unsigned int) _mm_cvtts_ss_epi32 (x);
}

unsigned long long
func4 (__m128 x)
{
  return (unsigned int) _mm_cvtts_roundss_epi32
    (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}

unsigned long long
func5 (__m128d x)
{
  return _mm_cvtts_sd_epu32 (x);
}

unsigned long long
func6 (__m128d x)
{
  return _mm_cvtts_roundsd_epu32
    (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}

unsigned long long
func7 (__m128d x)
{
  return (unsigned int) _mm_cvtts_sd_epi32 (x);
}

unsigned long long
func8 (__m128d x)
{
  return (unsigned int) _mm_cvtts_roundsd_epi32
    (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
