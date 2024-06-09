/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

#define T              uint8_t
#define RUN_SAT_BINARY RUN_SAT_U_ADD_FMT_6

DEF_SAT_U_ADD_FMT_6(T)

T test_data[][3] = {
  /* arg_0, arg_1, expect */
  {      0,     0,      0, },
  {      0,     1,      1, },
  {      1,     1,      2, },
  {      0,   254,    254, },
  {      1,   254,    255, },
  {      2,   254,    255, },
  {      0,   255,    255, },
  {      1,   255,    255, },
  {      2,   255,    255, },
  {    255,   255,    255, },
};

#include "scalar_sat_binary.h"
