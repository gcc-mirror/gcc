/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_add_uint32_t_fmt_6:
** slli\s+[atx][0-9]+,\s*a0,\s*32
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*32
** add\s+[atx][0-9]+,\s*a[01],\s*a[01]
** slli\s+[atx][0-9]+,\s*[atx][0-9],\s*32
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*32
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** or\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** sext.w\s+a0,\s*a0
** ret
*/
DEF_SAT_U_ADD_FMT_6(uint32_t)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */
