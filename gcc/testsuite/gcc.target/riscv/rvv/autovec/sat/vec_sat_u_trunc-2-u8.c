/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_TRUNC_FMT_1 (uint16_t, uint64_t)

/* { dg-final { scan-rtl-dump-times ".SAT_TRUNC " 2 "expand" } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 2 } } */
