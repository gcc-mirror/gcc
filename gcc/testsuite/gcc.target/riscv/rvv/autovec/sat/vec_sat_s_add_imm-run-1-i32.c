/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T int32_t
#define RUN(INDEX, T, out, in, expect, IMM, N) \
  RUN_VEC_SAT_S_ADD_IMM_FMT_1_WRAP (INDEX, T, out, in, expect, IMM, N)

DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(0, int32_t, uint32_t, -2147483648, INT32_MIN, INT32_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(1, int32_t, uint32_t, 0, INT32_MIN, INT32_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(2, int32_t, uint32_t, 1, INT32_MIN, INT32_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(3, int32_t, uint32_t, 2147483647, INT32_MIN, INT32_MAX)

int
main ()
{
  T out[N];
  T (*d)[2][N] = TEST_UNARY_DATA_WRAP (T, sat_s_add_imm);

  RUN (0, T, out, d[0][0], d[0][1], -2147483648, N);
  RUN (1, T, out, d[1][0], d[1][1], 0,    N);
  RUN (2, T, out, d[2][0], d[2][1], 1,    N);
  RUN (3, T, out, d[3][0], d[3][1], 2147483647,  N);

  return 0;
}
