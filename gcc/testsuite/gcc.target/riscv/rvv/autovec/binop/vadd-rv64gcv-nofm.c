/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include "vadd-template.h"

/* { dg-final { scan-assembler-times {\tvadd\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvadd\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvfadd\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvfadd\.vf} 6 } } */

/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 9 "optimized" } } */
