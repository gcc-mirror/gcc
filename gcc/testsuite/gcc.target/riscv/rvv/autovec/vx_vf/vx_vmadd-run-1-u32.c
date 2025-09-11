/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=gpr2vr-cost=0" } */

#include "vx_ternary.h"
#include "vx_ternary_data.h"

#define T          uint32_t
#define NAME       madd
#define TEST_DATA  TEST_TERNARY_DATA_WRAP(T, NAME)

DEF_VX_TERNARY_CASE_1_WRAP(T, *, +, NAME)

#define TEST_RUN(T, NAME, vd, vs2, rs1, n) \
  RUN_VX_TERNARY_CASE_1_WRAP(T, NAME, vd, vs2, rs1, n)

#include "vx_ternary_run.h"
