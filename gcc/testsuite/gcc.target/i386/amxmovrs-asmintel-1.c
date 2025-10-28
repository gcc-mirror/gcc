/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-movrs -masm=intel" } */
/* { dg-final { scan-assembler-times "tileloaddrs\[ \\t]tmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "tileloaddrst1\[ \\t]tmm\[0-9\]" 1 } } */
#include <immintrin.h>

extern const void* base;
extern const int stride;

#define TMM0 0
#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST()
{
  _tile_loaddrs (TMM1, base, stride);
  _tile_loaddrst1 (TMM1, base, stride);
}
