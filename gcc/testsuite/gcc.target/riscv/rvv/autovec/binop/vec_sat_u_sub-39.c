/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "vec_sat_arith.h"

/*
** vec_sat_u_sub_uint32_t_fmt_10:
** ...
** vsetvli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*e32,\s*m1,\s*ta,\s*ma
** ...
** vle32\.v\s+v[0-9]+,\s*0\([atx][0-9]+\)
** vle32\.v\s+v[0-9]+,\s*0\([atx][0-9]+\)
** vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** ...
*/
DEF_VEC_SAT_U_SUB_FMT_10(uint32_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
