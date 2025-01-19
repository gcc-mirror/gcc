/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 0)
DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 2)
DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 6)
DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 129)
DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 254)
DEF_SAT_U_SUB_IMM_FMT_4(uint8_t, 255)

#define T                       uint8_t
#define RUN(T, op, imm, expect) RUN_SAT_U_SUB_IMM_FMT_4(T, op, imm, expect)

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
  RUN (T,  d[0][0],   0,  d[0][2]);

  RUN (T,  d[1][0],   0,  d[1][2]);
  RUN (T,  d[2][0], 255,  d[2][2]);

  RUN (T,  d[3][0], 254,  d[3][2]);
  RUN (T,  d[4][0], 255,  d[4][2]);
  RUN (T,  d[5][0],   2,  d[5][2]);

  RUN (T,  d[6][0], 254,  d[6][2]);
  RUN (T,  d[7][0],   0,  d[7][2]);

  RUN (T,  d[8][0],   2,  d[8][2]);
  RUN (T,  d[9][0],   6,  d[9][2]);

  RUN (T, d[10][0],   0, d[10][2]);

  RUN (T, d[11][0], 129, d[11][2]);

  return 0;
}
