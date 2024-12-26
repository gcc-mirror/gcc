/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T int64_t
#define RUN(INDEX, T, out, in, expect, IMM, N) \
  RUN_VEC_SAT_S_ADD_IMM_FMT_1_WRAP (INDEX, T, out, in, expect, IMM, N)

DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(0, int64_t, uint64_t, INT64_MIN, INT64_MIN, INT64_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(1, int64_t, uint64_t, 0, INT64_MIN, INT64_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(2, int64_t, uint64_t, 1, INT64_MIN, INT64_MAX)
DEF_VEC_SAT_S_ADD_IMM_FMT_1_WRAP(3, int64_t, uint64_t, INT64_MAX, INT64_MIN, INT64_MAX)

int
main ()
{
  T out[N];
  T (*d)[2][N] = TEST_UNARY_DATA_WRAP (T, sat_s_add_imm);

  RUN (0, T, out, d[0][0], d[0][1], INT64_MIN, N);
  RUN (1, T, out, d[1][0], d[1][1], 0,    N);
  RUN (2, T, out, d[2][0], d[2][1], 1,    N);
  RUN (3, T, out, d[3][0], d[3][1], INT64_MAX,  N);

  return 0;
}
