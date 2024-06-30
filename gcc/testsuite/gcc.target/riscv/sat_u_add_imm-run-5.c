/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_FMT_2(uint8_t, 0)
DEF_SAT_U_ADD_IMM_FMT_2(uint8_t, 1)
DEF_SAT_U_ADD_IMM_FMT_2(uint8_t, 254)
DEF_SAT_U_ADD_IMM_FMT_2(uint8_t, 255)

#define T                       uint8_t
#define RUN(T, op, imm, expect) RUN_SAT_U_ADD_IMM_FMT_2(T, op, imm, expect)

T d[][3] = {
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

int
main ()
{
  RUN (T, d[0][0],   0, d[0][2]);

  RUN (T, d[1][0],   1, d[1][2]);
  RUN (T, d[2][0],   1, d[2][2]);

  RUN (T, d[3][0], 254, d[3][2]);
  RUN (T, d[4][0], 254, d[4][2]);
  RUN (T, d[5][0], 254, d[5][2]);

  RUN (T, d[6][0], 255, d[6][2]);
  RUN (T, d[7][0], 255, d[7][2]);
  RUN (T, d[8][0], 255, d[8][2]);
  RUN (T, d[9][0], 255, d[9][2]);

  return 0;
}
