/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_TRUNC_FMT_1 (uint8_t, uint16_t)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 2 "optimized" } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 1 } } */
