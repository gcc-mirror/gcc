/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T  int64_t
#define T1 int64_t
#define T2 uint64_t

DEF_VEC_SAT_S_SUB_FMT_3_WRAP (T1, T2, INT64_MIN, INT64_MAX)

#define test_data          TEST_BINARY_DATA_NAME_WRAP(T, T, sssub)
#define RUN_VEC_SAT_BINARY(T, out, op_1, op_2, N) \
  RUN_VEC_SAT_S_SUB_FMT_3_WRAP(T, out, op_1, op_2, N)

#include "vec_sat_binary_vvv_run.h"
