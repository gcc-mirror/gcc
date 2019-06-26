/* PR target/90991 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq -masm=att" } */
/* { dg-final { scan-assembler-times "vmovaps\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovups\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \t]\+\\(\[^\n\r]*\\), %xmm0" 1 } } */

#include <x86intrin.h>

__m512
f1 (void *a)
{
  return _mm512_insertf32x4 (_mm512_set1_ps (0.0f), _mm_load_ps (a), 0);
}

__m512d
f2 (void *a)
{
  return _mm512_insertf64x2 (_mm512_set1_pd (0.0), _mm_load_pd (a), 0);
}

__m512i
f3 (void *a)
{
  return _mm512_inserti32x4 (_mm512_set1_epi32 (0), _mm_load_si128 (a), 0);
}

__m512
f4 (void *a)
{
  return _mm512_insertf32x4 (_mm512_set1_ps (0.0f), _mm_loadu_ps (a), 0);
}

__m512d
f5 (void *a)
{
  return _mm512_insertf64x2 (_mm512_set1_pd (0.0), _mm_loadu_pd (a), 0);
}

__m512i
f6 (void *a)
{
  return _mm512_inserti32x4 (_mm512_set1_epi32 (0), _mm_loadu_si128 (a), 0);
}
