/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3" } */

#include "avg.h"
#include "avg_data.h"

#define WT   int32_t
#define NT   int8_t
#define NAME avg_floor

DEF_AVG_0_WRAP(NT, WT, NAME)

#define TEST_DATA                            TEST_AVG_DATA_WRAP(NT, NAME)
#define TEST_RUN(NT, WT, NAME, a, b, out, n) RUN_AVG_0_WRAP(NT, WT, NAME, a, b, out, n)

#include "avg_run.h"
