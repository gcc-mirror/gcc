/* { dg-do run } */
/* { dg-additional-options "-std=c99" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int64_t, uint64_t, (-9223372036854775807ll - 1), INT64_MIN, INT64_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int64_t, uint64_t, 9223372036854775807ll, INT64_MIN, INT64_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(2, int64_t, uint64_t, 100, INT64_MIN, INT64_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(3, int64_t, uint64_t, -100, INT64_MIN, INT64_MAX)

#define T                       int64_t
#define RUN(INDEX,T, x, expect) RUN_SAT_S_ADD_IMM_FMT_1(INDEX, T, x, expect)

T d[][2] = {
  /* arg_0,   expect */
  {     -1,     (-9223372036854775807ll - 1), },
  {      2,           -9223372036854775806ll, },
  {      1,            9223372036854775807ll, },
  {     -7,            9223372036854775800ll, },
  {      0,                              100, },
  {     -1,                               99, },
  {      0,                             -100, },
  {    100,                                0, },
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
