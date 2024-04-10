/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2" } */

#define TYPE uint64_t
#include "struct_vect-1.c"

/* { dg-final { scan-assembler-times {vlseg2e64\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg3e64\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg4e64\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg5e64\.v} 7 } } */
/* { dg-final { scan-assembler-times {vlseg6e64\.v} 7 } } */
/* { dg-final { scan-assembler-times {vlseg7e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vlseg8e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg2e64\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg3e64\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg4e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg5e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg6e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg7e64\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg8e64\.v} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*2,\s*e64,\s*m1,\s*t[au],\s*m[au]} 14 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
