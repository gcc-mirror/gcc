/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_FMT_4(int64_t, uint64_t, INT64_MIN, INT64_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" { target { no-opts "-O2" } } } } */
/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 4 "expand" { target { no-opts "-O3" } } } } */
/* { dg-final { scan-assembler-times {vsadd\.vv} 1 } } */
