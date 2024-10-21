/* { dg-do compile } */
/* { dg-skip-if  "" { *-*-* } { "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_sub_int8_t_fmt_2:
** sub\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*7
** andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
** srai\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*127
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** slliw\s+a0,\s*a0,\s*24
** sraiw\s+a0,\s*a0,\s*24
** ret
*/
DEF_SAT_S_SUB_FMT_2(int8_t, uint8_t, INT8_MIN, INT8_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
