/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_trunc_int32_t_to_int16_t_fmt_5:
** li\s+[atx][0-9]+,\s*32768
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** slt\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** li\s+[atx][0-9]+,\s*-32768
** slt\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** srai\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** xor\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** slliw\s+a0,\s*a0,\s*16
** sraiw\s+a0,\s*a0,\s*16
** ret
*/
DEF_SAT_S_TRUNC_FMT_5(int16_t, int32_t, INT16_MIN, INT16_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
