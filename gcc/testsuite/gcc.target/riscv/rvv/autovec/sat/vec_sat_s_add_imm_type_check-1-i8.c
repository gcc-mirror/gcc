/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_IMM_FMT_1(0, int8_t, uint8_t, 200, INT8_MIN, INT8_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1(1, int8_t, uint8_t, -300, INT8_MIN, INT8_MAX)

/* { dg-final { scan-tree-dump-not ".SAT_ADD " "optimized" } } */

