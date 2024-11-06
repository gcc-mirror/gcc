/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-tf32 -masm=intel" } */
/* { dg-final { scan-assembler "tmmultf32ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
#include <immintrin.h>

void TEST()
{
  _tile_mmultf32ps (1, 2, 3);
}
