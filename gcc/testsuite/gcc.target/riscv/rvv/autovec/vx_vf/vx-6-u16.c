/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

#define T uint16_t

DEF_VX_BINARY_CASE_1_WRAP(T, +, add, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_1_WRAP(T, -, sub, VX_BINARY_BODY_X8)
DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, -, rsub, VX_BINARY_REVERSE_BODY_X8);
DEF_VX_BINARY_CASE_1_WRAP(T, &, and, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_1_WRAP(T, |, or, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_1_WRAP(T, ^, xor, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_1_WRAP(T, /, div, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_1_WRAP(T, %, rem, VX_BINARY_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_0_WARP(T), max, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_1_WARP(T), max, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_0_WARP(T), min, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_1_WARP(T), min, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_U_ADD_FUNC_WRAP(T), sat_add, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_U_SUB_FUNC_WRAP(T), sat_sub, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_FLOOR_FUNC_WRAP(T), avg_floor, VX_BINARY_FUNC_BODY_X8)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_CEIL_FUNC_WRAP(T), avg_ceil, VX_BINARY_FUNC_BODY_X8)

/* { dg-final { scan-assembler {vadd.vx} } } */
/* { dg-final { scan-assembler {vsub.vx} } } */
/* { dg-final { scan-assembler {vrsub.vx} } } */
/* { dg-final { scan-assembler {vand.vx} } } */
/* { dg-final { scan-assembler {vor.vx} } } */
/* { dg-final { scan-assembler {vxor.vx} } } */
/* { dg-final { scan-assembler {vdivu.vx} } } */
/* { dg-final { scan-assembler {vremu.vx} } } */
/* { dg-final { scan-assembler {vmaxu.vx} } } */
/* { dg-final { scan-assembler {vminu.vx} } } */
/* { dg-final { scan-assembler {vsaddu.vx} } } */
/* { dg-final { scan-assembler {vssubu.vx} } } */
/* { dg-final { scan-assembler {vaaddu.vx} } } */
