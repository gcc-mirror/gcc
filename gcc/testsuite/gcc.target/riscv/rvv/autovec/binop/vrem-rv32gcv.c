/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax -fdump-tree-optimized-details" } */

#include "vrem-template.h"

/* { dg-final { scan-assembler-times {\tvrem\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvremu\.vv} 8 } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 16 "optimized" } } */
