/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-fp16 -masm=intel" } */
/* { dg-final { scan-assembler "tdpfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
#include <immintrin.h>

void TEST ()
{
  _tile_dpfp16ps (1, 2, 3);
}
