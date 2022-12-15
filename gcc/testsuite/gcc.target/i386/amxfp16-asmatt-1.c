/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-fp16" } */
/* { dg-final { scan-assembler "tdpfp16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
#include <immintrin.h>

#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST ()
{
  _tile_dpfp16ps (TMM1, TMM2, TMM3);
}
