/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

#define T int32_t

DEF_VX_BINARY_CASE_1_WRAP(T, +, add, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, -, sub, VX_BINARY_BODY_X4)
DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, -, rsub, VX_BINARY_REVERSE_BODY_X4);
DEF_VX_BINARY_CASE_1_WRAP(T, &, and, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, |, or, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, ^, xor, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, *, mul, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, /, div, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, %, rem, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_0_WARP(T), max, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, MAX_FUNC_1_WARP(T), max, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_0_WARP(T), min, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, MIN_FUNC_1_WARP(T), min, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_S_ADD_FUNC_WRAP(T), sat_add, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, SAT_S_SUB_FUNC_WRAP(T), sat_sub, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_FLOOR_FUNC_WRAP(T), avg_floor, VX_BINARY_FUNC_BODY_X4)
DEF_VX_BINARY_CASE_3_WRAP(T, AVG_CEIL_FUNC_WRAP(T), avg_ceil, VX_BINARY_FUNC_BODY_X4)

/* { dg-final { scan-assembler {vadd.vx} } } */
/* { dg-final { scan-assembler {vsub.vx} } } */
/* { dg-final { scan-assembler {vrsub.vx} } } */
/* { dg-final { scan-assembler {vand.vx} } } */
/* { dg-final { scan-assembler {vor.vx} } } */
/* { dg-final { scan-assembler {vxor.vx} } } */
/* { dg-final { scan-assembler {vmul.vx} } } */
/* { dg-final { scan-assembler {vdiv.vx} } } */
/* { dg-final { scan-assembler {vrem.vx} } } */
/* { dg-final { scan-assembler {vmax.vx} } } */
/* { dg-final { scan-assembler {vmin.vx} } } */
/* { dg-final { scan-assembler-not {vsadd.vx} } } */
/* { dg-final { scan-assembler-not {vssub.vx} } } */
/* { dg-final { scan-assembler {vaadd.vx} { target { any-opts {
     "-mrvv-vector-bits=zvl -mrvv-max-lmul=m1"
     "-mrvv-vector-bits=zvl -mrvv-max-lmul=m2"
     "-mrvv-vector-bits=zvl -mrvv-max-lmul=m4"
   } } } } } */
