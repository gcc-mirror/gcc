/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_add_imm_int8_t_fmt_1_0:
**	addi\s+[atx][0-9]+,\s*a0,\s*9
**	xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
**	srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*7
**	srli\s+[atx][0-9]+,\s*a0,\s*7
**	xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
**	andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	srai\s+a0,\s*a0,\s*63
**	xori\s+[atx][0-9]+,\s*a0,\s*127
**	neg\s+a0,\s*[atx][0-9]+
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
**	addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
**	and\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
**	or\s+a0,\s*a0,\s*[atx][0-9]+
**	slliw\s+a0,\s*a0,\s*24
**	sraiw\s+a0,\s*a0,\s*24
**	ret
*/
DEF_SAT_S_ADD_IMM_FMT_1(0, int8_t, uint8_t, 9, INT8_MIN, INT8_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */
