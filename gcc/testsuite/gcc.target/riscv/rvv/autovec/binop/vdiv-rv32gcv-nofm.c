/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include "vdiv-template.h"

/* { dg-final { scan-assembler-times {\tvdiv\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvdivu\.vv} 8 } } */

/* { dg-final { scan-assembler-times {\tvfdiv\.vv} 6 } } */

/* { dg-final { scan-tree-dump-times "\.COND_LEN_DIV" 16 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_RDIV" 6 "optimized" } } */
/* { dg-final { scan-assembler-not {\tvmv1r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv2r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv4r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv8r\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv\.v\.v} } } */
/* { dg-final { scan-assembler-not {\tvmv\.v\.i} } } */
