/* { dg-do compile } */
/* { dg-skip-if  "" { *-*-* } { "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_add_imm3_uint16_t_fmt_1:
** addi\s+[atx][0-9]+,\s*a0,\s*3
** slli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*48
** srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*48
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** or\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** slli\s+a0,\s*a0,\s*48
** srli\s+a0,\s*a0,\s*48
** ret
*/
DEF_SAT_U_ADD_IMM_FMT_1(uint16_t, 3)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
