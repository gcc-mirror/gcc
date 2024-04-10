/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized" } */

#include "extract_last-13.c"

/* { dg-final { scan-tree-dump "\.LEN_FOLD_EXTRACT_LAST" "optimized" } } */
