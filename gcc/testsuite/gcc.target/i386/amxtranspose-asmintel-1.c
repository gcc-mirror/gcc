/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-transpose -mamx-bf16 -mamx-complex -mamx-fp16 -mamx-tf32 -masm=intel" } */
/* { dg-final { scan-assembler "ttdpbf16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "ttdpfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "ttransposed\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0t1\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1t1\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tconjtcmmimfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "tconjtfp16\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2"  } } */
/* { dg-final { scan-assembler "ttcmmimfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "ttcmmrlfp16ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
/* { dg-final { scan-assembler "ttmmultf32ps\[ \\t]+\[^\n\]*%tmm1+\[^\n\]*%tmm2+\[^\n\]*%tmm3"  } } */
#include <immintrin.h>

extern const void* base;
extern const int stride;

void TEST()
{
  _tile_tdpbf16ps (1, 2, 3);
  _tile_tdpfp16ps (1, 2, 3);
  _tile_transposed (1, 2);
  _tile_2rpntlvwz0 (5, base, stride);
  _tile_2rpntlvwz0t1 (4, base, stride);
  _tile_2rpntlvwz1 (3, base, stride);
  _tile_2rpntlvwz1t1 (2, base, stride);
  _tile_conjtcmmimfp16ps (1, 2, 3);
  _tile_conjtfp16 (1, 2);
  _tile_tcmmimfp16ps (1, 2, 3);
  _tile_tcmmrlfp16ps (1, 2, 3);
  _tile_tmmultf32ps (1, 2, 3);
}
