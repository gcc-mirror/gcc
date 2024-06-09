/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gc_zve64d_zvfh -mabi=ilp32d -O3" } */

#include "pr111037-1.c"

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*1,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 } } */
