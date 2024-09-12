/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../vec_sat_arith.h"

/*
** vec_sat_s_add_int64_t_fmt_1:
** ...
** vsetvli\s+[atx][0-9]+,\s*zero,\s*e64,\s*m1,\s*ta,\s*ma
** ...
** vsadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** ...
*/
DEF_VEC_SAT_S_ADD_FMT_1(int64_t, uint64_t, INT64_MIN, INT64_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
