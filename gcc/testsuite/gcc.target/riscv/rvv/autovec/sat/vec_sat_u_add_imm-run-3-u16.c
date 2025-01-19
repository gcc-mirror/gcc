/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#include "vec_sat_arith.h"
#include "vec_sat_data.h"

#define T uint16_t
#define RUN(T, out, in, expect, IMM, N) \
  RUN_VEC_SAT_U_ADD_IMM_FMT_3_WRAP (T, out, in, expect, IMM, N)

DEF_VEC_SAT_U_ADD_IMM_FMT_3_WRAP (T,     0u)
DEF_VEC_SAT_U_ADD_IMM_FMT_3_WRAP (T,     1u)
DEF_VEC_SAT_U_ADD_IMM_FMT_3_WRAP (T, 65534u)
DEF_VEC_SAT_U_ADD_IMM_FMT_3_WRAP (T, 65535u)

int
main ()
{
  T out[N];
  T (*d)[2][N] = TEST_UNARY_DATA_WRAP (T, sat_u_add_imm);

  RUN (T, out, d[0][0], d[0][1],     0u, N);
  RUN (T, out, d[1][0], d[1][1],     1u, N);
  RUN (T, out, d[2][0], d[2][1], 65534u, N);
  RUN (T, out, d[3][0], d[3][1], 65535u, N);

  return 0;
}
