/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_IMM_FMT_3(uint16_t, 65530)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
