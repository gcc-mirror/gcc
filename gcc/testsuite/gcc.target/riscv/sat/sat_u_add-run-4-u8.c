/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"
#include "sat_arith_data.h"

#define T1               uint8_t
#define DATA             TEST_BINARY_DATA_WRAP(T1, usadd)
#define T                TEST_BINARY_STRUCT_DECL(T1, usadd)

DEF_SAT_U_ADD_FMT_4_WRAP(T1)

#define RUN_BINARY(x, y) RUN_SAT_U_ADD_FMT_4_WRAP(T1, x, y)

#include "scalar_sat_binary_run_xxx.h"
