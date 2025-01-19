/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

#define T              uint32_t
#define RUN_SAT_BINARY RUN_SAT_U_SUB_FMT_1

DEF_SAT_U_SUB_FMT_1(T)

T test_data[][3] = {
  /*     arg_0,      arg_1,      expect */
  {          0,          0,           0, },
  {          0,          1,           0, },
  {          1,          1,           0, },
  { 4294967295, 4294967294,           1, },
  { 4294967295, 4294967295,           0, },
  { 4294967294, 4294967295,           0, },
  { 4294967293, 4294967294,           0, },
  {          1, 4294967295,           0, },
  {          2, 4294967295,           0, },
  {          5,          1,           4, },
};

#include "scalar_sat_binary.h"
