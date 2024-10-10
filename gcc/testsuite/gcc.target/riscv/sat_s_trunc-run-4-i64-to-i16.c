/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"
#include "sat_arith_data.h"

#define T1 int16_t
#define T2 int64_t

DEF_SAT_S_TRUNC_FMT_4_WRAP(T1, T2, INT16_MIN, INT16_MAX)

#define DATA           TEST_UNARY_DATA_WRAP(T1, T2)
#define T              TEST_UNARY_STRUCT_DECL(T1, T2)
#define RUN_UNARY(x)   RUN_SAT_S_TRUNC_FMT_4_WRAP(T1, T2, x)

#include "scalar_sat_unary.h"
