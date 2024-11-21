/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T1 uint16_t
#define T2 uint32_t

DEF_VEC_SAT_U_SUB_ZIP_WRAP(T1, T2)

#define DATA                    TEST_BINARY_DATA_NAME_WRAP(T1, T2, zip)
#define T                       TEST_ZIP_STRUCT_DECL(T1, T2)
#define RUN_BINARY_VX(x, b, N)  RUN_VEC_SAT_U_SUB_FMT_ZIP_WRAP(T1, T2, x, b, N)

#include "vec_sat_binary_vx_run.h"
