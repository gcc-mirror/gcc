/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_trunc_int16_t_to_int8_t_fmt_4:
** slti\s+[atx][0-9]+,\s*[atx][0-9]+,\s*127
** li\s+[atx][0-9]+,\s*-128
** slt\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** srai\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*127
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** slliw\s+a0,\s*a0,\s*24
** sraiw\s+a0,\s*a0,\s*24
** ret
*/
DEF_SAT_S_TRUNC_FMT_4(int8_t, int16_t, INT8_MIN, INT8_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_TRUNC " 2 "expand" } } */
