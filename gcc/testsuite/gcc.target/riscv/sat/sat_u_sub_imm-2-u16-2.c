/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm65533_uint16_t_fmt_2:
** li\s+[atx][0-9]+,\s*65536
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-3
** sub\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
** sltu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** addi\s+a0,\s*a0,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** slli\s+a0,\s*a0,\s*48
** srli\s+a0,\s*a0,\s*48
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_2(uint16_t, 65533)

/* { dg-final { scan-tree-dump-times ".SAT_SUB " 1 "optimized" } } */
