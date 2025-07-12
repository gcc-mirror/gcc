/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_2(0, int32_t, uint32_t, 10, INT32_MIN, INT32_MAX)

DEF_SAT_S_ADD_IMM_FMT_2(1, int32_t, uint32_t, -1, INT32_MIN, INT32_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
/* { dg-final { scan-assembler-not "\.L\[0-9\]+" } } */
