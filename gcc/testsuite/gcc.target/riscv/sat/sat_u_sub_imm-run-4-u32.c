/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 0)
DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 2)
DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 6)
DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 2147483647)
DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 4294967294)
DEF_SAT_U_SUB_IMM_FMT_4(uint32_t, 4294967295)

#define T                       uint32_t
#define RUN(T, op, imm, expect) RUN_SAT_U_SUB_IMM_FMT_4(T, op, imm, expect)

T d[][3] = {
  /* arg_0, arg_1, expect */
  {          0,          0,          0, },
  {          1,          0,          1, },
  {          1, 4294967295,          0, },
  { 4294967294, 4294967294,          0, },
  { 4294967294, 4294967295,          0, },
  { 4294967294,          2, 4294967292, },
  { 4294967295, 4294967294,          1, },
  { 4294967295,          0, 4294967295, },
  {          5,          2,          3, },
  {          5,          6,          0, },
  { 2147483647,          0, 2147483647, },
  { 2147483648, 2147483647,          1, },
};

int
main ()
{
  RUN (T,  d[0][0],          0,  d[0][2]);

  RUN (T,  d[1][0],          0,  d[1][2]);
  RUN (T,  d[2][0], 4294967295,  d[2][2]);

  RUN (T,  d[3][0], 4294967294,  d[3][2]);
  RUN (T,  d[4][0], 4294967295,  d[4][2]);
  RUN (T,  d[5][0],          2,  d[5][2]);

  RUN (T,  d[6][0], 4294967294,  d[6][2]);
  RUN (T,  d[7][0],          0,  d[7][2]);

  RUN (T,  d[8][0],          2,  d[8][2]);
  RUN (T,  d[9][0],          6,  d[9][2]);

  RUN (T, d[10][0],          0, d[10][2]);
  RUN (T, d[11][0], 2147483647, d[11][2]);

  return 0;
}
