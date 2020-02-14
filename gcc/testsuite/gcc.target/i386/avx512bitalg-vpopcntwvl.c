/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bitalg -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

extern __m256i y, y_1;
extern __m128i x, x_1;

int foo ()
{
  __mmask16 msk16;
  __mmask8 msk8;
  __m256i c256 = _mm256_popcnt_epi16 (y);
  asm volatile ("" : "+v" (c256));
  c256 = _mm256_mask_popcnt_epi16 (y_1, msk16, y);
  asm volatile ("" : "+v" (c256));
  c256 = _mm256_maskz_popcnt_epi16 (msk16, y);
  asm volatile ("" : "+v" (c256));
  __m128i c128 = _mm_popcnt_epi16 (x);
  asm volatile ("" : "+v" (c128));
  c128 = _mm_mask_popcnt_epi16 (x_1, msk8, x);
  asm volatile ("" : "+v" (c128));
  c128 = _mm_maskz_popcnt_epi16 (msk8, x);
  asm volatile ("" : "+v" (c128));
}
