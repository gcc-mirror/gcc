/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d --param=riscv-autovec-preference=scalable -fdump-tree-optimized-details" } */

#include "vadd-template.h"

/* { dg-final { scan-assembler-times {\tvadd\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvadd\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvfadd\.vv} 7 } } */
/* There are 2 MINUS operations.  */
/* { dg-final { scan-assembler-times {\tvfsub\.vv} 2 } } */

/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 2 "optimized" } } */
