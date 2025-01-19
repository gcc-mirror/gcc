/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm1_uint32_t_fmt_2:
** snez\s+[atx][0-9]+,\s*a0
** subw\s+a0,\s*a0,\s*[atx][0-9]+
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_2(uint32_t, 1)

/* { dg-final { scan-tree-dump-not ".SAT_SUB" "optimized" } } */
