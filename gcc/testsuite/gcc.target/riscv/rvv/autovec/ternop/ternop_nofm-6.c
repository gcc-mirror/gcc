/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=scalable -fdump-tree-optimized-details -fno-schedule-insns" } */

#include "ternop-6.c"

/* { dg-final { scan-tree-dump-times "COND_LEN_FNMA" 3 "optimized" } } */
