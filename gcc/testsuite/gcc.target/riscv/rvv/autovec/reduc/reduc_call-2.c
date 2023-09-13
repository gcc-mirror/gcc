/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=scalable -fno-vect-cost-model -ffast-math" } */

#include "reduc_call-1.c"

/* { dg-final { scan-assembler-times {vfmadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 1 } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
