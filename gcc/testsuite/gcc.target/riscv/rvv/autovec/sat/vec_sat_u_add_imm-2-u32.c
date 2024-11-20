/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_IMM_FMT_2(uint32_t, 33)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 4 "expand" } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv} 1 } } */
