/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details" } */

#include "../vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_FMT_4(int8_t, uint8_t, INT8_MIN, INT8_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
/* { dg-final { scan-assembler-times {vsadd\.vv} 1 } } */
