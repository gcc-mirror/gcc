/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T1 int32_t
#define T2 int64_t

DEF_VEC_SAT_S_TRUNC_FMT_5_WRAP(T1, T2, INT32_MIN, INT32_MAX)

#define T                     TEST_UNARY_STRUCT_DECL(T1, T2)
#define DATA                  TEST_UNARY_DATA_WRAP(T1, T2)
#define RUN_UNARY(out, in, N) RUN_VEC_SAT_S_TRUNC_FMT_5_WRAP(T1, T2, out, in, N)

#include "vec_sat_unary_vv_run.h"
