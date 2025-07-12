/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_FMT_4(int32_t, uint32_t, INT32_MIN, INT32_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */
/* { dg-final { scan-assembler-not "\.L\[0-9\]+" } } */
