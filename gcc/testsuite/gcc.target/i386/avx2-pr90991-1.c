/* PR target/90991 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -masm=att" } */
/* { dg-final { scan-assembler-times "vmovaps\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovups\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-not "vmovaps\[^\n\r]*xmm0\[^\n\r]*xmm0" } } */
/* { dg-final { scan-assembler-not "vmovapd\[^\n\r]*xmm0\[^\n\r]*xmm0" } } */
/* { dg-final { scan-assembler-not "vmovdqa\[^\n\r]*xmm0\[^\n\r]*xmm0" } } */

#include <x86intrin.h>

__m256
f1 (void *a)
{
  return _mm256_insertf128_ps (_mm256_set1_ps (0.0f), _mm_load_ps (a), 0);
}

__m256d
f2 (void *a)
{
  return _mm256_insertf128_pd (_mm256_set1_pd (0.0), _mm_load_pd (a), 0);
}

__m256i
f3 (void *a)
{
  return _mm256_insertf128_si256 (_mm256_set1_epi32 (0), _mm_load_si128 (a), 0);
}

__m256
f4 (void *a)
{
  return _mm256_insertf128_ps (_mm256_set1_ps (0.0f), _mm_loadu_ps (a), 0);
}

__m256d
f5 (void *a)
{
  return _mm256_insertf128_pd (_mm256_set1_pd (0.0), _mm_loadu_pd (a), 0);
}

__m256i
f6 (void *a)
{
  return _mm256_insertf128_si256 (_mm256_set1_epi32 (0), _mm_loadu_si128 (a), 0);
}
