/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#define TYPE uint16_t
#define ITYPE int16_t
#include "struct_vect-6.c"

/* { dg-final { scan-assembler-times {vlseg2e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg3e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg5e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg6e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg7e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg8e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg2e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg3e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg4e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg5e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg6e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg7e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg8e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+} 14 } } */
