/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

/*
** vec_sat_u_add_uint16_t_fmt_3:
** ...
** vsetvli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*e16,\s*m1,\s*ta,\s*ma
** ...
** vsaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** ...
*/
DEF_VEC_SAT_U_ADD_FMT_3(uint16_t)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv} 1 } } */
