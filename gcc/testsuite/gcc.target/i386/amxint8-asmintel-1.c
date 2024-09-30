/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-int8 -masm=intel" } */
/* { dg-final { scan-assembler "tdpbssd\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdpbsud\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdpbusd\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tdpbuud\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
#include <immintrin.h>

void TEST ()
{
  _tile_dpbssd (1, 2, 3);
  _tile_dpbsud (1, 2, 3);
  _tile_dpbusd (1, 2, 3);
  _tile_dpbuud (1, 2, 3);
}
