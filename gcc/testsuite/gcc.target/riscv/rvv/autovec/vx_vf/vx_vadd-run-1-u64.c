/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=gpr2vr-cost=0" } */

#include "vx_binary.h"
#include "vx_binary_data.h"

#define T uint64_t

DEF_VX_BINARY_CASE_0_WRAP(T, +)

#define TEST_DATA               TEST_BINARY_DATA_WRAP(T, vadd)
#define TEST_RUN(out, in, x, n) RUN_VX_BINARY_CASE_0_WRAP(out, in, x, n)

#include "vx_binary_run.h"
