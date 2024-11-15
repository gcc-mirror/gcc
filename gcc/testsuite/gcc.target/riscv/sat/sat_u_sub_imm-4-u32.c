/* { dg-do compile } */
/* { dg-skip-if  "" { *-*-* } { "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sat_arith.h"

/*
** sat_u_sub_imm255_uint32_t_fmt_4:
** slli\s+a0,\s*a0,\s*32
** srli\s+a0,\s*a0,\s*32
** addi\s+[atx][0-9]+,\s*a0,\s*-255
** sltiu\s+a0,\s*[atx][0-9]+,\s*255
** addi\s+a0,\s*a0,\s*-1
** and\s+a0,\s*a0,\s*[atx][0-9]+
** sext\.w\s+a0,\s*a0
** ret
*/

DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 255)

/* { dg-final { scan-rtl-dump-times ".SAT_SUB " 2 "expand" } } */
