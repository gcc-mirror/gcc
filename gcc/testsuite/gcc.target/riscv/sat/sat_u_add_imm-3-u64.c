/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_add_imm8ull_uint64_t_fmt_3:
** addi\s+[atx][0-9]+,\s*a0,\s*8
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** or\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** ret
*/
DEF_SAT_U_ADD_IMM_FMT_3(uint64_t, 8ull)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
