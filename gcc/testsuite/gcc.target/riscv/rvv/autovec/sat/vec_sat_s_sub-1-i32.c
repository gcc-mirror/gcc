/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details" } */

#include "../vec_sat_arith.h"

DEF_VEC_SAT_S_SUB_FMT_1(int32_t, uint32_t, INT32_MIN, INT32_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
/* { dg-final { scan-assembler-times {vssub\.vv} 1 } } */
