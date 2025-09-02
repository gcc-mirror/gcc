/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

#define T uint64_t

DEF_SAT_U_MUL_FMT_2_WRAP(T)

/* { dg-final { scan-tree-dump-times ".SAT_MUL" 1 "optimized" } } */
/* { dg-final { scan-assembler-not "\.L\[0-9\]+" } } */
