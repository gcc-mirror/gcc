/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_IMM_FMT_1(0, int32_t, uint32_t, -2147483649, INT32_MIN, INT32_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1(1, int32_t, uint32_t, 2147483648, INT32_MIN, INT32_MAX)

/* { dg-final { scan-tree-dump-not ".SAT_ADD " "optimized" } } */
