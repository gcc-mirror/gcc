/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 0)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 1)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 5)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 32767)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 32768)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 65534)
DEF_SAT_U_SUB_IMM_FMT_1(uint16_t, 65535)

#define T                       uint16_t
#define RUN(T, imm, op, expect) RUN_SAT_U_SUB_IMM_FMT_1(T, imm, op, expect)

T d[][3] = {
  /* arg_0, arg_1, expect */
  {        0,     0,      0, },
  {        1,     0,      1, },
  {        1, 65535,      0, },
  {    65534, 65534,      0, },
  {    65534, 65535,      0, },
  {    65534,     2,  65532, },
  {    65535, 65534,      1, },
  {    65535,     0,  65535, },
  {        5,     2,      3, },
  {        5,     6,      0, },
  {    32767,     0,  32767, },
  {    32768, 32767,      1, },
};

int
main ()
{
  RUN (T,     0, d[0][1], d[0][2]);

  RUN (T,     1, d[1][1], d[1][2]);
  RUN (T,     1, d[2][1], d[2][2]);

  RUN (T, 65534, d[3][1], d[3][2]);
  RUN (T, 65534, d[4][1], d[4][2]);
  RUN (T, 65534, d[5][1], d[5][2]);

  RUN (T, 65535, d[6][1], d[6][2]);
  RUN (T, 65535, d[7][1], d[7][2]);

  RUN (T,     5, d[8][1], d[8][2]);
  RUN (T,     5, d[9][1], d[9][2]);

  RUN (T, 32767, d[10][1], d[10][2]);

  RUN (T, 32768, d[11][1], d[11][2]);

  return 0;
}
