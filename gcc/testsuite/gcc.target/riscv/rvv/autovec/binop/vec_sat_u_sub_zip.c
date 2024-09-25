/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details" } */

#include "../vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_ZIP_WRAP(uint16_t, uint32_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 1 } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 1 } } */
