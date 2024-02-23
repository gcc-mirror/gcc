/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -fno-vect-cost-model -fdump-tree-optimized" } */

#define TYPE uint8_t

#include "extract_last-3.c"

/* { dg-final { scan-tree-dump "\.LEN_FOLD_EXTRACT_LAST" "optimized" } } */
