/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-int8" } */
/* { dg-final { scan-assembler "tdpbssd\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "tdpbsud\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } *
/* { dg-final { scan-assembler "tdpbusd\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "tdpbuud\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
#include <immintrin.h>

#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST ()
{
  _tile_dpbssd (TMM1, TMM2, TMM3);
  _tile_dpbsud (TMM1, TMM2, TMM3);
  _tile_dpbusd (TMM1, TMM2, TMM3);
  _tile_dpbuud (TMM1, TMM2, TMM3);
}
