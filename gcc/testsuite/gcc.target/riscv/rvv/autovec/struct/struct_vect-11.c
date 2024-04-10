/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#define TYPE float
#define ITYPE int32_t
#include "struct_vect-6.c"

/* { dg-final { scan-assembler-times {vlseg2e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg3e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg5e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg6e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg7e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg8e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg2e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg3e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg4e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg5e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg6e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg7e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg8e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+} 14 } } */
