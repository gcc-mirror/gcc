/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2" } */

#define TYPE uint16_t
#include "struct_vect-1.c"

/* { dg-final { scan-assembler-times {vlseg2e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg3e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg4e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg5e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg6e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vlseg7e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vlseg8e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg2e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg3e16\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg4e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg5e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg6e16\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg7e16\.v} 2 } } */
/* { dg-final { scan-assembler-times {vsseg8e16\.v} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*8,\s*e16,\s*m1,\s*t[au],\s*m[au]} 14 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
