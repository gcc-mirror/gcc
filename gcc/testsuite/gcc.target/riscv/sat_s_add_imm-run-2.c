/* { dg-do run } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int16_t, uint16_t, -32768, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int16_t, uint16_t, 32767, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(2, int16_t, uint16_t, 100, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(3, int16_t, uint16_t, -100, INT16_MIN, INT16_MAX)

#define T                       int16_t
#define RUN(INDEX,T, x, expect) RUN_SAT_S_ADD_IMM_FMT_1(INDEX, T, x, expect)

T d[][2] = {
  /* arg_0,   expect */
  {     -1,     -32768, },
  {      2,     -32766, },
  {      1,      32767, },
  {    -10,      32757, },
  {  32669,      32767, },
  { -32768,     -32668, },
  { -32768,     -32768, },
  {      0,       -100, },
};

int
main ()
{
  RUN (0, T, d[0][0], d[0][1]);
  RUN (0, T, d[1][0], d[1][1]);

  RUN (1, T, d[2][0], d[2][1]);
  RUN (1, T, d[3][0], d[3][1]);

  RUN (2, T, d[4][0], d[4][1]);
  RUN (2, T, d[5][0], d[5][1]);

  RUN (3, T, d[6][0], d[6][1]);
  RUN (3, T, d[7][0], d[7][1]);

  return 0;
}
