/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2" } */

#define TYPE uint32_t
#include "struct_vect-1.c"

/* { dg-final { scan-assembler-times {vlseg2e32\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg3e32\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg4e32\.v} 8 } } */
/* { dg-final { scan-assembler-times {vlseg5e32\.v} 6 } } */
/* { dg-final { scan-assembler-times {vlseg6e32\.v} 6 } } */
/* { dg-final { scan-assembler-times {vlseg7e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vlseg8e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg2e32\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg3e32\.v} 8 } } */
/* { dg-final { scan-assembler-times {vsseg4e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg5e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg6e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg7e32\.v} 4 } } */
/* { dg-final { scan-assembler-times {vsseg8e32\.v} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 14 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
