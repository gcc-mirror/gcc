/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_IMM_FMT_1(0, int32_t, uint32_t, 9, INT32_MIN, INT32_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" { target { no-opts "-O2" } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" { target { no-opts "-O3" } } } } */
/* { dg-final { scan-assembler-times {vsadd\.vi} 1 } } */
