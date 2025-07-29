/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-additional-options "-std=c99 -O3 -Wno-pedantic" } */

#include "avg.h"
#include "avg_data.h"

#define WT   int128_t
#define NT   int64_t
#define NAME avg_ceil

DEF_AVG_1_WRAP(NT, WT, NAME)

#define TEST_DATA                            TEST_AVG_DATA_WRAP(NT, NAME)
#define TEST_RUN(NT, WT, NAME, a, b, out, n) RUN_AVG_1_WRAP(NT, WT, NAME, a, b, out, n)

#include "avg_run.h"
