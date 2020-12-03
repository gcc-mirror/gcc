/* PR target/97770 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bitalg -mavx512vl -mprefer-vector-width=512" } */
/* Add xfail since no IFN for QI/HImode popcount */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*xmm" 1 {xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*xmm" 1 {xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*ymm" 1 {xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*ymm" 1 {xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \\t\]+\[^\\n\\r\]*zmm" 1 {xfail *-*-*} } } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \\t\]+\[^\\n\\r\]*zmm" 1 {xfail *-*-*} } } */

#include <immintrin.h>

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_128 (char * __restrict dest, char* src)
{
  for (int i = 0; i != 16; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_128 (short* __restrict dest, short* src)
{
  for (int i = 0; i != 8; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_256 (char * __restrict dest, char* src)
{
  for (int i = 0; i != 32; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_256 (short* __restrict dest, short* src)
{
  for (int i = 0; i != 16; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountb_512 (char * __restrict dest, char* src)
{
  for (int i = 0; i != 64; i++)
    dest[i] = __builtin_popcount (src[i]);
}

void
__attribute__ ((noipa, optimize("-O3")))
popcountw_512 (short* __restrict dest, short* src)
{
  for (int i = 0; i != 32; i++)
    dest[i] = __builtin_popcount (src[i]);
}
