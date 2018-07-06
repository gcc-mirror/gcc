/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vpopcntdq -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

extern __m128i x, x_1;
extern __m256i y, y_1;
extern __m512i z, z_1;

int foo ()
{
  __mmask16 msk;
  __mmask8 msk8;
  __m128i a = _mm_popcnt_epi32 (x);
  asm volatile ("" : "+v" (a));
  a = _mm_mask_popcnt_epi32 (x, msk8, x_1);
  asm volatile ("" : "+v" (a));
  a = _mm_maskz_popcnt_epi32 (msk8, x);
  asm volatile ("" : "+v" (a));
  __m256i b = _mm256_popcnt_epi32 (y);
  asm volatile ("" : "+v" (b));
  b = _mm256_mask_popcnt_epi32 (y, msk8, y_1);
  asm volatile ("" : "+v" (b));
  b = _mm256_maskz_popcnt_epi32 (msk8, y);
  asm volatile ("" : "+v" (b));
  __m512i c = _mm512_popcnt_epi32 (z);
  asm volatile ("" : "+v" (c));
  c = _mm512_mask_popcnt_epi32 (z, msk, z_1);
  asm volatile ("" : "+v" (c));
  c = _mm512_maskz_popcnt_epi32 (msk, z);
  asm volatile ("" : "+v" (c));
}
