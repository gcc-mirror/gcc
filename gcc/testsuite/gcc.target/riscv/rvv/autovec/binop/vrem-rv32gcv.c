/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl -fdump-tree-optimized-details" } */

#include "vrem-template.h"

/* { dg-final { scan-assembler-times {\tvrem\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvremu\.vv} 8 } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 16 "optimized" } } */
/* { dg-final { scan-assembler-not {\tvmv1r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv2r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv4r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv8r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv\.v\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv\.v\.i} } } */
