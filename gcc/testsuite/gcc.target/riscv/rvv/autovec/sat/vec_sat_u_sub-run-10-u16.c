/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T                  uint16_t

DEF_VEC_SAT_U_SUB_FMT_10_WRAP(T)

#define test_data          TEST_UNARY_DATA_WRAP(T, ussub)
#define RUN_VEC_SAT_BINARY(T, out, op_1, op_2, N) \
  RUN_VEC_SAT_U_SUB_FMT_10_WRAP(T, out, op_1, op_2, N)

#include "vec_sat_binary_vvv_run.h"
