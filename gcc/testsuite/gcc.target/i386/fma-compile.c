/* Test that the compiler properly generates floating point multiply
   and add instructions FMA systems.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mfma" } */

#include <x86intrin.h>

__m128d
check_mm_fmadd_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmadd_pd (a, b, c);
}

__m256d
check_mm256_fmadd_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fmadd_pd (a, b, c);
}

__m128
check_mm_fmadd_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmadd_ps (a, b, c);
}

__m256
check_mm256_fmadd_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fmadd_ps (a, b, c);
}

__m128d
check_mm_fmadd_sd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmadd_sd (a, b, c);
}

__m128
check_mm_fmadd_ss (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmadd_ss (a, b, c);
}

__m128d
check_mm_fmsub_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmsub_pd (a, b, c);
}

__m256d
check_mm256_fmsub_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fmsub_pd (a, b, c);
}

__m128
check_mm_fmsub_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmsub_ps (a, b, c);
}

__m256
check_mm256_fmsub_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fmsub_ps (a, b, c);
}

__m128d
check_mm_fmsub_sd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmsub_sd (a, b, c);
}

__m128
check_mm_fmsub_ss (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmsub_ss (a, b, c);
}

__m128d
check_mm_fnmadd_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fnmadd_pd (a, b, c);
}

__m256d
check_mm256_fnmadd_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fnmadd_pd (a, b, c);
}

__m128
check_mm_fnmadd_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fnmadd_ps (a, b, c);
}

__m256
check_mm256_fnmadd_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fnmadd_ps (a, b, c);
}

__m128d
check_mm_fnmadd_sd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fnmadd_sd (a, b, c);
}

__m128
check_mm_fnmadd_ss (__m128 a, __m128 b, __m128 c)
{
  return _mm_fnmadd_ss (a, b, c);
}

__m128d
check_mm_fnmsub_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fnmsub_pd (a, b, c);
}

__m256d
check_mm256_fnmsub_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fnmsub_pd (a, b, c);
}

__m128
check_mm_fnmsub_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fnmsub_ps (a, b, c);
}

__m256
check_mm256_fnmsub_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fnmsub_ps (a, b, c);
}

__m128d
check_mm_fnmsub_sd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fnmsub_sd (a, b, c);
}

__m128
check_mm_fnmsub_ss (__m128 a, __m128 b, __m128 c)
{
  return _mm_fnmsub_ss (a, b, c);
}

__m128d
check_mm_fmaddsub_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmaddsub_pd (a, b, c);
}

__m256d
check_mm256_fmaddsub_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fmaddsub_pd (a, b, c);
}

__m128
check_mm_fmaddsub_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmaddsub_ps (a, b, c);
}

__m256
check_mm256_fmaddsub_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fmaddsub_ps (a, b, c);
}

__m128d
check_mm_fmsubadd_pd (__m128d a, __m128d b, __m128d c)
{
  return _mm_fmsubadd_pd (a, b, c);
}

__m256d
check_mm256_fmsubadd_pd (__m256d a, __m256d b, __m256d c)
{
  return _mm256_fmsubadd_pd (a, b, c);
}

__m128
check_mm_fmsubadd_ps (__m128 a, __m128 b, __m128 c)
{
  return _mm_fmsubadd_ps (a, b, c);
}

__m256
check_mm256_fmsubadd_ps (__m256 a, __m256 b, __m256 c)
{
  return _mm256_fmsubadd_ps (a, b, c);
}


/* { dg-final { scan-assembler-times "vfmadd\[^s\]..ps" 2 } } */
/* { dg-final { scan-assembler-times "vfmsub\[^s\]..ps" 2 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ps" 2 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ps" 2 } } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps" 2 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...ps" 2 } } */
/* { dg-final { scan-assembler-times "vfmadd\[^s\]..pd" 2 } } */
/* { dg-final { scan-assembler-times "vfmsub\[^s\]..pd" 2 } } */
/* { dg-final { scan-assembler-times "vfnmadd...pd" 2 } } */
/* { dg-final { scan-assembler-times "vfnmsub...pd" 2 } } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd" 2 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd" 2 } } */
/* { dg-final { scan-assembler-times "vfmadd\[^s\]..ss" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub\[^s\]..ss" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ss" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ss" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd\[^s\]..sd" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub\[^s\]..sd" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sd" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd" 1 } } */
