/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fdump-rtl-expand-details -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../vec_sat_arith.h"

/*
** vec_sat_u_add_imm9_uint8_t_fmt_1:
** ...
** vsaddu\.vi\s+v[0-9]+,\s*v[0-9]+,\s*9
** ...
*/
DEF_VEC_SAT_U_ADD_IMM_FMT_1(uint8_t, 9)
