/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_sub_int64_t_fmt_2:
** sub\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** srai\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** li\s+[atx][0-9]+,\s*-1
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
** xor\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** ret
*/
DEF_SAT_S_SUB_FMT_2(int64_t, uint64_t, INT64_MIN, INT64_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" } } */
