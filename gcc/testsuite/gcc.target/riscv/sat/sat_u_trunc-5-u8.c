/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_trunc_uint64_t_to_uint8_t_fmt_3:
** sltiu\s+[atx][0-9]+,\s*a0,\s*255
** addi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*-1
** or\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** andi\s+[atx][0-9]+,\s*[atx][0-9]+,\s*0xff
** ret
*/
DEF_SAT_U_TRUNC_FMT_3(uint8_t, uint64_t)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
