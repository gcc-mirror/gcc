/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_FMT_3(uint16_t, 0)
DEF_SAT_U_ADD_IMM_FMT_3(uint16_t, 1)
DEF_SAT_U_ADD_IMM_FMT_3(uint16_t, 65534)
DEF_SAT_U_ADD_IMM_FMT_3(uint16_t, 65535)

#define T                       uint16_t
#define RUN(T, op, imm, expect) RUN_SAT_U_ADD_IMM_FMT_3(T, op, imm, expect)

T d[][3] = {
  /* arg_0, arg_1, expect */
  {      0,     0,      0, },
  {      0,     1,      1, },
  {      1,     1,      2, },
  {      0, 65534,  65534, },
  {      1, 65534,  65535, },
  {      2, 65534,  65535, },
  {      0, 65535,  65535, },
  {      1, 65535,  65535, },
  {      2, 65535,  65535, },
  {  65535, 65535,  65535, },
};

int
main ()
{
  RUN (T, d[0][0],     0, d[0][2]);

  RUN (T, d[1][0],     1, d[1][2]);
  RUN (T, d[2][0],     1, d[2][2]);

  RUN (T, d[3][0], 65534, d[3][2]);
  RUN (T, d[4][0], 65534, d[4][2]);
  RUN (T, d[5][0], 65534, d[5][2]);

  RUN (T, d[6][0], 65535, d[6][2]);
  RUN (T, d[7][0], 65535, d[7][2]);
  RUN (T, d[8][0], 65535, d[8][2]);
  RUN (T, d[9][0], 65535, d[9][2]);

  return 0;
}
