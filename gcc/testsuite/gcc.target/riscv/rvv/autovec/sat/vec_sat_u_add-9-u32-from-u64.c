/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_FMT_9(uint64_t, uint32_t)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv} 1 } } */
