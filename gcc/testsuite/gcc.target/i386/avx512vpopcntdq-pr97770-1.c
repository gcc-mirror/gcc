/* PR target/97770 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=icelake-server -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\\n\\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\\n\\r\]*ymm" 1 } } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \\t\]+\[^\\n\\r\]*zmm" 1 } } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\\n\\r\]*xmm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\\n\\r\]*ymm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \\t\]+\[^\\n\\r\]*zmm" 1  } } */
#ifndef AVX512VPOPCNTQ_H_INCLUDED
#define AVX512VPOPCNTQ_H_INCLUDED

#include <immintrin.h>

void
__attribute__ ((noipa, optimize("-O3")))
popcountd_128 (int* __restrict dest, int* src)
{
  for (int i = 0; i != 4; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountq_128 (long long* __restrict dest, long long* src)
{
  for (int i = 0; i != 2; i++)
    dest[i] = __builtin_popcountll (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountd_256 (int* __restrict dest, int* src)
{
  for (int i = 0; i != 8; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountq_256 (long long* __restrict dest, long long* src)
{
  for (int i = 0; i != 4; i++)
    dest[i] = __builtin_popcountll (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountd_512 (int* __restrict dest, int* src)
{
  for (int i = 0; i != 16; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountq_512 (long long* __restrict dest, long long* src)
{
  for (int i = 0; i != 8; i++)
    dest[i] = __builtin_popcountll (src[i]);
}
#endif
