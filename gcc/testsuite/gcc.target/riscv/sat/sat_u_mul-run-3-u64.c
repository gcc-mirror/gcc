/* { dg-do run { target { rv32 || rv64 } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"
#include "sat_arith_data.h"

#define NT               uint64_t
#define NAME             usmul
#define DATA             TEST_BINARY_DATA_WRAP(NT, NAME)
#define T                TEST_BINARY_STRUCT_DECL_WRAP(NT, NAME)
#define RUN_BINARY(x, y) RUN_SAT_U_MUL_FMT_2_WRAP(NT, x, y)

DEF_SAT_U_MUL_FMT_2_WRAP(NT)

#include "scalar_sat_binary_run_xxx.h"
