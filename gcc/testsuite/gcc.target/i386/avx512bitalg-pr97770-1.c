/* PR target/97770 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=icelake-server -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*xmm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*xmm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*ymm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*ymm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*zmm" 1  } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*zmm" 1  } } */

#include <immintrin.h>

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_128 (unsigned char * __restrict dest, unsigned char* src)
{
  for (int i = 0; i != 16; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_128 (unsigned short* __restrict dest, unsigned short* src)
{
  for (int i = 0; i != 8; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_256 (unsigned char * __restrict dest, unsigned char* src)
{
  for (int i = 0; i != 32; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_256 (unsigned short* __restrict dest, unsigned short* src)
{
  for (int i = 0; i != 16; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_512 (unsigned char * __restrict dest, unsigned char* src)
{
  for (int i = 0; i != 64; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_512 (unsigned short* __restrict dest, unsigned short* src)
{
  for (int i = 0; i != 32; i++)
    dest[i] = __builtin_popcount (src[i]);
}
