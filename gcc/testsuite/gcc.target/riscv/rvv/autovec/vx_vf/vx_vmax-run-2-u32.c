/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=gpr2vr-cost=0" } */

#include "vx_binary.h"
#include "vx_binary_data.h"

#define T          uint32_t
#define NAME       max
#define FUNC       MAX_FUNC_1_WARP(T)
#define TEST_DATA  TEST_BINARY_DATA_WRAP(T, NAME)

DEF_VX_BINARY_CASE_2_WRAP(T, FUNC, max)

#define TEST_RUN(T, NAME, out, in, x, n) \
  RUN_VX_BINARY_CASE_2_WRAP(T, NAME, FUNC, out, in, x, n)

#include "vx_binary_run.h"
