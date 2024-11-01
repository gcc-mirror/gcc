/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-movrs -mamx-transpose" } */
/* { dg-final { scan-assembler "tileloaddrs\[ \\t]+\[^\n\]*\(%\[a-z0-9\]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tileloaddrst1\[ \\t]+\[^\n\]*\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0rs\[ \\t]+\[^\n\]*\(%\[a-z0-9\]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0rst1\[ \\t]+\[^\n\]*\(%\[a-z0-9\]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1rs\[ \\t]+\[^\n\]*\(%\[a-z0-9\]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1rst1\[ \\t]+\[^\n\]*\(%\[a-z0-9\]*\,%\[a-z0-9\]*\,\[124\]\)+\[^\n\]*%tmm\[0-9\]"  } } */
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
  _tile_2rpntlvwz0rs (TMM0, base, stride);
  _tile_2rpntlvwz0rst1 (TMM1, base, stride);
  _tile_2rpntlvwz1rs (TMM2, base, stride);
  _tile_2rpntlvwz1rst1 (TMM3, base, stride);
}
