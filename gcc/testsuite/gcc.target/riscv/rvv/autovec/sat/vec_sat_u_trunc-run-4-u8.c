/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T1 uint8_t
#define T2 uint16_t

DEF_VEC_SAT_U_TRUNC_FMT_3_WRAP(T1, T2)

#define T                     TEST_UNARY_STRUCT_DECL(T1, T2)
#define DATA                  TEST_UNARY_DATA_WRAP(T1, T2)
#define RUN_UNARY(out, in, N) RUN_VEC_SAT_U_TRUNC_FMT_3_WRAP(T1, T2, out, in, N)

#include "vec_sat_unary_vv_run.h"
