/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_IMM_FMT_3(uint8_t, 9u)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
/* { dg-final { scan-assembler-times {vsaddu\.vi} 1 } } */
