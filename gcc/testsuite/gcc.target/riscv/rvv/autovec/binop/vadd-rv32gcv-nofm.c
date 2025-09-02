/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details -fno-pie" } */

#include "vadd-template.h"

/* { dg-final { scan-assembler-times {\tvadd\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvadd\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvfadd\.vv} 9 } } */
/* { dg-final { scan-assembler-not {\tvfadd\.vf} } } */

/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 9 "optimized" } } */
