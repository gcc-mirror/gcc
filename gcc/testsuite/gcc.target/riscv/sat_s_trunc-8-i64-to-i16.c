/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_trunc_int64_t_to_int16_t_fmt_8:
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
DEF_SAT_S_TRUNC_FMT_8(int16_t, int64_t, INT16_MIN, INT16_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_TRUNC " 2 "expand" } } */
