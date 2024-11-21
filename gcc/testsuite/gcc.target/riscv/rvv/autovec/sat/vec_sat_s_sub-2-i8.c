/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-rtl-expand-details" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_SUB_FMT_2(int8_t, uint8_t, INT8_MIN, INT8_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" { target { any-opts
     "-O3"
   } } } } */
/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 4 "expand" { target { any-opts
     "-O2"
   } } } } */
/* { dg-final { scan-assembler-times {vssub\.vv} 1 } } */
