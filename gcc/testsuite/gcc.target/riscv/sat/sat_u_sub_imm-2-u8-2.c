/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm253_uint8_t_fmt_2:
** addi\s+[atx][0-9]+,\s*a0,\s*-253
** sltiu\s+a0,\s*[atx][0-9]+,\s*253
** addi\s+a0,\s*a0,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** andi\s+a0,\s*a0,\s*0xff
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_2(uint8_t, 253)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" } } */
