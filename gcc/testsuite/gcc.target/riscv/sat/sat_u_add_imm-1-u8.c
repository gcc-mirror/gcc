/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_add_imm9_uint8_t_fmt_1:
** addi\s+[atx][0-9]+,\s*a0,\s*9
** andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*0xff
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** neg\s+[atx][0-9]+,\s*[atx][0-9]+
** or\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** andi\s+a0,\s*a0,\s*0xff
** ret
*/
DEF_SAT_U_ADD_IMM_FMT_1(uint8_t, 9)

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
