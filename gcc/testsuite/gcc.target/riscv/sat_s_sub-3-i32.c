/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_sub_int32_t_fmt_3:
** sub\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*a1
** xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** srliw\s+[atx][0-9]+,\s*[atx][0-9]+,\s*31
** srai\s+[atx][0-9]+,\s*[atx][0-9]+,\s*63
** li\s+[atx][0-9]+,\s*-2147483648
** xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** xor\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-7]
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** sext\.w\s+a0,\s*[atx][0-9]+
** ret
*/
DEF_SAT_S_SUB_FMT_3(int32_t, uint32_t, INT32_MIN, INT32_MAX)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
