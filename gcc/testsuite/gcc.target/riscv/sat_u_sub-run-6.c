/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

#define T              uint16_t
#define RUN_SAT_BINARY RUN_SAT_U_SUB_FMT_2

DEF_SAT_U_SUB_FMT_2(T)

T test_data[][3] = {
  /* arg_0, arg_1, expect */
  {      0,     0,      0, },
  {      0,     1,      0, },
  {      1,     1,      0, },
  {  65535, 65534,      1, },
  {  65535, 65535,      0, },
  {  65534, 65535,      0, },
  {  65533, 65534,      0, },
  {      0, 65535,      0, },
  {      1, 65535,      0, },
  {     35,     5,     30, },
};

#include "scalar_sat_binary.h"
