/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_uint8_t_fmt_9:
** sub\s+[atx][0-9]+,\s*a0,\s*a1
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** andi\s+a0,\s*a0,\s*0xff
** ret
*/
DEF_SAT_U_SUB_FMT_9(uint8_t)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
