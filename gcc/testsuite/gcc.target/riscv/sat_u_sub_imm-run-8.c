/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 0)
DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 1)
DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 2)
DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 6)
DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 18446744073709551614u)
DEF_SAT_U_SUB_IMM_FMT_2(uint64_t, 18446744073709551615u)

#define T                       uint64_t
#define RUN(T, imm, op, expect) RUN_SAT_U_SUB_IMM_FMT_2(T, imm, op, expect)

T d[][3] = {
  /* arg_0, arg_1, expect */
  {                     0,                     0,                     0, },
  {                     1,                     0,                     1, },
  {                     1, 18446744073709551615u,                     0, },
  { 18446744073709551614u, 18446744073709551614u,                     0, },
  { 18446744073709551614u, 18446744073709551615u,                     0, },
  { 18446744073709551614u,                     2, 18446744073709551612u, },
  { 18446744073709551615u, 18446744073709551614u,                     1, },
  { 18446744073709551615u,                     0, 18446744073709551615u, },
  {                     5,                     2,                     3, },
  {                     5,                     6,                     0, },
};

int
main ()
{
  RUN (T, d[0][0],                     0, d[0][2]);

  RUN (T, d[1][0],                     0, d[1][2]);
  RUN (T, d[2][0], 18446744073709551615u, d[2][2]);

  RUN (T, d[3][0], 18446744073709551614u, d[3][2]);
  RUN (T, d[4][0], 18446744073709551615u, d[4][2]);
  RUN (T, d[5][0],                     2, d[5][2]);

  RUN (T, d[6][0], 18446744073709551614u, d[6][2]);
  RUN (T, d[7][0],                     0, d[7][2]);

  RUN (T, d[8][0],                     2, d[8][2]);
  RUN (T, d[9][0],                     6, d[9][2]);

  return 0;
}
