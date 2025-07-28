/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

#define T uint64_t

DEF_VX_BINARY_CASE_1_WRAP(T, +, add, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, -, sub, VX_BINARY_BODY)
DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, -, rsub, VX_BINARY_REVERSE_BODY);
DEF_VX_BINARY_CASE_1_WRAP(T, &, and, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, |, or, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, ^, xor, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, /, div, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, %, rem, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_0_WARP(T), max, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_1_WARP(T), max, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_0_WARP(T), min, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_1_WARP(T), min, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_U_ADD_FUNC_WRAP(T), sat_add, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_U_SUB_FUNC_WRAP(T), sat_sub, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_FLOOR_FUNC_WRAP(T), avg_floor, VX_BINARY_FUNC_BODY)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_CEIL_FUNC_WRAP(T), avg_ceil, VX_BINARY_FUNC_BODY)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
/* { dg-final { scan-assembler-not {vdivu.vx} } } */
/* { dg-final { scan-assembler-not {vremu.vx} } } */
/* { dg-final { scan-assembler-not {vmaxu.vx} } } */
/* { dg-final { scan-assembler-not {vminu.vx} } } */
/* { dg-final { scan-assembler-not {vsaddu.vx} } } */
/* { dg-final { scan-assembler-not {vssubu.vx} } } */
/* { dg-final { scan-assembler-not {vaaddu.vx} } } */
