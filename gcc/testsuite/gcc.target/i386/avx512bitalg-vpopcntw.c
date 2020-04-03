/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bitalg -mavx512bw" } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

extern __m512i z, z1;

int foo ()
{
  __mmask16 msk;
  __m512i c = _mm512_popcnt_epi16 (z);
  asm volatile ("" : "+v" (c));
  c = _mm512_mask_popcnt_epi16 (z1, msk, z);
  asm volatile ("" : "+v" (c));
  c = _mm512_maskz_popcnt_epi16 (msk, z);
  asm volatile ("" : "+v" (c));
}
