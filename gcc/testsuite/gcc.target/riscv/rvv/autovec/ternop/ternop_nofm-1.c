/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=scalable -fdump-tree-optimized-details" } */

#include "ternop-1.c"

/* { dg-final { scan-assembler-not {\tvmv} } } */
/* { dg-final { scan-tree-dump-times "COND_LEN_FMA" 3 "optimized" } } */
