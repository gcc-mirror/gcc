/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_trunc_uint64_t_to_uint32_t_fmt_1:
** li\s+[atx][0-9]+,\s*-1
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*32
** sltu\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** or\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** sext.w\s+a0,\s*a0
** ret
*/
DEF_SAT_U_TRUNC_FMT_1(uint32_t, uint64_t)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
