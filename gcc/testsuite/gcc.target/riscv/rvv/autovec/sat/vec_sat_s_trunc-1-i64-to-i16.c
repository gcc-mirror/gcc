/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_TRUNC_FMT_1(int16_t, int64_t, INT16_MIN, INT16_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vnclip\.wi} 2 } } */
