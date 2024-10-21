/* { dg-do compile } */
/* { dg-skip-if  "" { *-*-* } { "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_uint32_t_fmt_12:
** slli\s+a0,\s*a0,\s*32
** srli\s+a0,\s*a0,\s*32
** slli\s+a1,\s*a1,\s*32
** srli\s+a1,\s*a1,\s*32
** sub\s+[atx][0-9]+,\s*a0,\s*a1
** sltu\s+[atx][0-9]+,\s*a0,\s*a1
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** and\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
** sext.w\s+a0,\s*a0
** ret
*/
DEF_SAT_U_SUB_FMT_12(uint32_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
