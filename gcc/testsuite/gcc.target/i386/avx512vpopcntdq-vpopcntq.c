/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vpopcntdq" } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

extern __m512i z, z1;

int foo ()
{
  __mmask8 msk; 
  __m512i c = _mm512_popcnt_epi64 (z);
  asm volatile ("" : "+v" (c));
  c = _mm512_mask_popcnt_epi64 (z, msk, z1);
  asm volatile ("" : "+v" (c));
  c = _mm512_maskz_popcnt_epi64 (msk, z);  
  asm volatile ("" : "+v" (c));
}
