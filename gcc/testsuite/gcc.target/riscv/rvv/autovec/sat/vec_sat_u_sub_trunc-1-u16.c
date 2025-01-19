/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_TRUNC_FMT_1(uint16_t, uint32_t)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vssubu\.vx} 1 } } */
/* { dg-final { scan-assembler-times {vncvt\.x\.x\.w} 1 } } */
