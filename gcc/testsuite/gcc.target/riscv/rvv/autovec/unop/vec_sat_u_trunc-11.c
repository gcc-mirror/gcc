/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../vec_sat_arith.h"

/*
** vec_sat_u_trunc_uint16_t_uint64_t_fmt_2:
** ...
** vsetvli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*e32,\s*mf2,\s*ta,\s*ma
** vle64\.v\s+v[0-9]+,\s*0\([atx][0-9]+\)
** vnclipu\.wi\s+v[0-9]+,\s*v[0-9]+,\s*0
** vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*ta,\s*ma
** vnclipu\.wi\s+v[0-9]+,\s*v[0-9]+,\s*0
** vse16\.v\s+v[0-9]+,\s*0\([atx][0-9]+\)
** ...
*/
DEF_VEC_SAT_U_TRUNC_FMT_2 (uint16_t, uint64_t)

/* { dg-final { scan-rtl-dump-times ".SAT_TRUNC " 2 "expand" } } */
