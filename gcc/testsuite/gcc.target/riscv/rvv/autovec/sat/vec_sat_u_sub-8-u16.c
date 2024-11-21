/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_FMT_8(uint16_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 4 "expand" } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 1 } } */
