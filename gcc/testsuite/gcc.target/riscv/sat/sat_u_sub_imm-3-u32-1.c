/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm2147483649_uint32_t_fmt_3:
** li\s+[atx][0-9]+,\s*1
** slli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*31
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
** slli\s+a0,\s*a0,\s*32
** srli\s+a0,\s*a0,\s*32
** sub\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+a0,\s*a0,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** sext\.w\s+a0,\s*a0
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 2147483649)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
