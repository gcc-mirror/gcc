/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_s_add_imm_int32_t_fmt_1_0:
**	addi\s+[atx][0-9]+,\s*a0,\s*10
**	xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
**	srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*31
**	srli\s+[atx][0-9]+,\s*a0,\s*31
**	xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
**	andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	srai\s+a0,\s*a0,\s*63
**	li\s+[atx][0-9]+,\s*-2147483648
**	xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
**	xor\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
**	neg\s+a0,\s*[atx][0-9]+
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
**	addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
**	and\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
**	or\s+a0,a0,\s*[atx][0-9]+
**	sext.w\s+a0,\s*a0
**	ret
*/
DEF_SAT_S_ADD_IMM_FMT_1(0, int32_t, uint32_t, 10, INT32_MIN, INT32_MAX)

/*
** sat_s_add_imm_int32_t_fmt_1_1:
**	addi\s+[atx][0-9]+,\s*a0,\s*-1
**	not\s+[atx][0-9]+,\s*a0
**	xor\s+[atx][0-9]+,\s*a0,\s*[atx][0-9]+
**	srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*31
**	srli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*31
**	xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
**	andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*1
**	srai\s+a0,\s*a0,\s*63
**	li\s+[atx][0-9]+,\s*-2147483648
**	xori\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
**	xor\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
**	neg\s+a0,\s*[atx][0-9]+
**	and\s+[atx][0-9]+,\s*[atx][0-9]+,\s*a0
**	addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
**	and\s+a0,\s*[atx][0-9]+,\s*[atx][0-9]+
**	or\s+a0,\s*a0,\s*[atx][0-9]+
**	sext.w\s+a0,\s*a0
**	ret
*/
DEF_SAT_S_ADD_IMM_FMT_1(1, int32_t, uint32_t, -1, INT32_MIN, INT32_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
