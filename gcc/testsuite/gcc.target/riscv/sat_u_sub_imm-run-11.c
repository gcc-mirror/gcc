/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 0)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 1)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 5)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 2147483647)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 2147483648)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 4294967294)
DEF_SAT_U_SUB_IMM_FMT_3(uint32_t, 4294967295)

#define T                       uint32_t
#define RUN(T, imm, op, expect) RUN_SAT_U_SUB_IMM_FMT_3(T, imm, op, expect)

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
  RUN (T,          0,  d[0][1],  d[0][2]);

  RUN (T,          1,  d[1][1],  d[1][2]);
  RUN (T,          1,  d[2][1],  d[2][2]);

  RUN (T, 4294967294,  d[3][1],  d[3][2]);
  RUN (T, 4294967294,  d[4][1],  d[4][2]);
  RUN (T, 4294967294,  d[5][1],  d[5][2]);

  RUN (T, 4294967295,  d[6][1],  d[6][2]);
  RUN (T, 4294967295,  d[7][1],  d[7][2]);

  RUN (T,          5,  d[8][1],  d[8][2]);
  RUN (T,          5,  d[9][1],  d[9][2]);

  RUN (T, 2147483647, d[10][1], d[10][2]);
  RUN (T, 2147483648, d[11][1], d[11][2]);

  return 0;
}
