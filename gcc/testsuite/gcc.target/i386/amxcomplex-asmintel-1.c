/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-complex -masm=intel" } */
/* { dg-final { scan-assembler "tcmmimfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tcmmrlfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
#include <immintrin.h>

void TEST()
{
  _tile_cmmimfp16ps (1, 2, 3);
  _tile_cmmrlfp16ps (1, 2, 3);
}
