/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include "reduc-5.c"

/* { dg-final { scan-tree-dump-times "VEC_SHL_INSERT" 8 "optimized" } } */
