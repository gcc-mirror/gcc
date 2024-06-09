/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#define TYPE uint64_t
#define ITYPE int64_t
#include "struct_vect-6.c"

/* { dg-final { scan-assembler-times {vlseg2e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg3e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg5e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg6e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg7e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg8e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg2e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg3e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg4e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg5e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg6e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg7e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg8e64\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+} 14 } } */
