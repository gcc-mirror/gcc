/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

#define T              uint8_t
#define RUN_SAT_BINARY RUN_SAT_U_SUB_FMT_5

DEF_SAT_U_SUB_FMT_5(T)

T test_data[][3] = {
  /* arg_0, arg_1, expect */
  {      0,     0,      0, },
  {      0,     1,      0, },
  {      1,     1,      0, },
  {    255,   254,      1, },
  {    255,   255,      0, },
  {    254,   255,      0, },
  {    253,   254,      0, },
  {      0,   255,      0, },
  {      1,   255,      0, },
  {     32,     5,     27, },
};

#include "scalar_sat_binary.h"
