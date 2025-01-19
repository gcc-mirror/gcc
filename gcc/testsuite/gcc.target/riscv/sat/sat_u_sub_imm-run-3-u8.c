/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 0)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 1)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 5)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 127)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 128)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 254)
DEF_SAT_U_SUB_IMM_FMT_3(uint8_t, 255)

#define T                       uint8_t
#define RUN(T, imm, op, expect) RUN_SAT_U_SUB_IMM_FMT_3(T, imm, op, expect)

T d[][3] = {
  /* arg_0, arg_1, expect */
  {      0,     0,      0, },
  {      1,     0,      1, },
  {      1,   255,      0, },
  {    254,   254,      0, },
  {    254,   255,      0, },
  {    254,     2,    252, },
  {    255,   254,      1, },
  {    255,     0,    255, },
  {      5,     2,      3, },
  {      5,     6,      0, },
  {    127,     0,    127, },
  {    128,   129,      0, },
};

int
main ()
{
  RUN (T,   0,  d[0][1],  d[0][2]);

  RUN (T,   1,  d[1][1],  d[1][2]);
  RUN (T,   1,  d[2][1],  d[2][2]);

  RUN (T, 254,  d[3][1],  d[3][2]);
  RUN (T, 254,  d[4][1],  d[4][2]);
  RUN (T, 254,  d[5][1],  d[5][2]);

  RUN (T, 255,  d[6][1],  d[6][2]);
  RUN (T, 255,  d[7][1],  d[7][2]);

  RUN (T,   5,  d[8][1],  d[8][2]);
  RUN (T,   5,  d[9][1],  d[9][2]);

  RUN (T, 127, d[10][1], d[10][2]);

  RUN (T, 128, d[11][1], d[11][2]);

  return 0;
}
