/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_ADD_IMM_FMT_3(uint32_t, 9294967495ull)

/* { dg-final { scan-tree-dump-not ".SAT_ADD " "optimized" } } */
