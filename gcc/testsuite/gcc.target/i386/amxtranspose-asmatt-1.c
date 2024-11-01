/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-transpose -mamx-bf16 -mamx-complex -mamx-fp16 -mamx-tf32" } */
/* { dg-final { scan-assembler "ttdpbf16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "ttdpfp16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "ttransposed\[ \\t]+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz0t1\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "t2rpntlvwz1t1\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tconjtcmmimfp16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "tconjtfp16\[ \\t]+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "ttcmmimfp16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "ttcmmrlfp16ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
/* { dg-final { scan-assembler "ttmmultf32ps\[ \\t]+\[^\n\]*%tmm3+\[^\n\]*%tmm2+\[^\n\]*%tmm1"  } } */
#include <immintrin.h>

extern const void* base;
extern const int stride;

#define TMM0 0
#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST()
{
  _tile_tdpbf16ps (TMM1, TMM2, TMM3);
  _tile_tdpfp16ps (TMM1, TMM2, TMM3);
  _tile_transposed (TMM1, TMM2);
  _tile_2rpntlvwz0 (TMM0, base, stride);
  _tile_2rpntlvwz0t1 (TMM1, base, stride);
  _tile_2rpntlvwz1 (TMM2, base, stride);
  _tile_2rpntlvwz1t1 (TMM3, base, stride);
  _tile_conjtcmmimfp16ps (TMM1, TMM2, TMM3);
  _tile_conjtfp16 (TMM1, TMM2);
  _tile_tcmmimfp16ps (TMM1, TMM2, TMM3);
  _tile_tcmmrlfp16ps (TMM1, TMM2, TMM3);
  _tile_tmmultf32ps (TMM1, TMM2, TMM3);
}
