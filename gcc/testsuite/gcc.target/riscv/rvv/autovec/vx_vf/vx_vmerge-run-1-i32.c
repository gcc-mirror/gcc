/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=gpr2vr-cost=0" } */

#include "vx_binary.h"
#include "vx_binary_data.h"

#define T    int32_t
#define NAME merge

DEF_VX_MERGE_0_WRAP(T)

#define TEST_DATA                        TEST_BINARY_DATA_WRAP(T, NAME)
#define TEST_RUN(T, NAME, out, in, x, n) RUN_VX_MERGE_0_WRAP(T, out, in, x, n)

#include "vx_binary_run.h"
