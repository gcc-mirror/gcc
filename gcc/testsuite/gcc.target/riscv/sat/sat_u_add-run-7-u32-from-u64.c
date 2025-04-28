/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

#define T              uint32_t
#define WT             uint64_t
#define RUN_SAT_BINARY RUN_SAT_U_ADD_FMT_7_FROM_U64_WRAP

DEF_SAT_U_ADD_FMT_7_WRAP(WT, T)

T test_data[][3] = {
  /*     arg_0,      arg_1,      expect */
  {          0,          0,           0, },
  {          0,          1,           1, },
  {          1,          1,           2, },
  {          0, 4294967294,  4294967294, },
  {          1, 4294967294,  4294967295, },
  {          2, 4294967294,  4294967295, },
  {          0, 4294967295,  4294967295, },
  {          1, 4294967295,  4294967295, },
  {          2, 4294967295,  4294967295, },
  { 4294967295, 4294967295,  4294967295, },
};

#include "scalar_sat_binary.h"
