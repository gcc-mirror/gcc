/* { dg-do compile } */
/* { dg-skip-if  "" { *-*-* } { "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm253_uint8_t_fmt_3:
** li\s+[atx][0-9]+,\s*253
** sub\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+a0,\s*a0,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** andi\s+a0,\s*a0,\s*0xff
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 253)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
