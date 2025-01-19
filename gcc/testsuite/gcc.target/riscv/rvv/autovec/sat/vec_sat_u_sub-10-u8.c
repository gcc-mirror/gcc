/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_FMT_10(uint8_t)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 2 "optimized" } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 1 } } */
