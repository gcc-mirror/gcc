/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_U_SUB_FMT_2(uint64_t)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 2 "optimized" { target { no-opts
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m1"
   } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" { target { any-opts
     "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m1"
   } } } } */
/* { dg-final { scan-assembler-times {vssubu\.vv} 1 } } */
