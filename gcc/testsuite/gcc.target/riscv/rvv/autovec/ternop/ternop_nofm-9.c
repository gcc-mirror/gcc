/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details -fno-schedule-insns -fno-vect-cost-model" } */

#include "ternop-9.c"

/* { dg-final { scan-tree-dump-times "COND_LEN_FMS" 9 "optimized" } } */
/* { dg-final { scan-assembler-times {\tvmv} 3 } } */
