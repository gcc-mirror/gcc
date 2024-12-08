/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_SUB_FMT_2(int64_t, uint64_t, INT64_MIN, INT64_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" { target { any-opts
     "-O3"
   } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_SUB " 2 "optimized" { target { any-opts
     "-O2"
   } } } } */
/* { dg-final { scan-assembler-times {vssub\.vv} 1 } } */
