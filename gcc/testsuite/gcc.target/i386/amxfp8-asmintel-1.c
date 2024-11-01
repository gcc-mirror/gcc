/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-fp16 -masm=intel" } */
/* { dg-final { scan-assembler "tdpbf8ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdpbhf8ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdphbf8ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdphf8ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */

#include <immintrin.h>

#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST ()
{
  _tile_dpbf8ps (TMM1, TMM2, TMM3);
  _tile_dpbhf8ps (TMM1, TMM2, TMM3);
  _tile_dphbf8ps (TMM1, TMM2, TMM3);
  _tile_dphf8ps (TMM1, TMM2, TMM3);
}
