/* PR target/90991 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq -masm=att -mtune=intel" } */
/* { dg-final { scan-assembler-times "vmovaps\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovups\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \t]\+\\(\[^\n\r]*\\), %ymm0" 1 } } */

#include <x86intrin.h>

__m512
f1 (void *a)
{
  return _mm512_insertf32x8 (_mm512_set1_ps (0.0f), _mm256_load_ps (a), 0);
}

__m512d
f2 (void *a)
{
  return _mm512_insertf64x4 (_mm512_set1_pd (0.0), _mm256_load_pd (a), 0);
}

__m512i
f3 (void *a)
{
  return _mm512_inserti32x8 (_mm512_set1_epi32 (0), _mm256_load_si256 (a), 0);
}

__m512
f4 (void *a)
{
  return _mm512_insertf32x8 (_mm512_set1_ps (0.0f), _mm256_loadu_ps (a), 0);
}

__m512d
f5 (void *a)
{
  return _mm512_insertf64x4 (_mm512_set1_pd (0.0), _mm256_loadu_pd (a), 0);
}

__m512i
f6 (void *a)
{
  return _mm512_inserti32x8 (_mm512_set1_epi32 (0), _mm256_loadu_si256 (a), 0);
}
