#ifndef HAVE_DEFINED_VX_VF_TERNARY_H
#define HAVE_DEFINED_VX_VF_TERNARY_H

#include <stdint.h>

#undef HAS_INT128

#if __riscv_xlen == 64
#define HAS_INT128
typedef unsigned __int128 uint128_t;
typedef signed __int128 int128_t;
#endif

#define DEF_VX_TERNARY_CASE_0(T, OP_1, OP_2, NAME)                        \
T                                                                         \
test_vx_ternary_##NAME##_##T##_case_0 (T * restrict vd, T * restrict vs2, \
                                       T rs1, unsigned n)                 \
{                                                                         \
  unsigned i;                                                             \
                                                                          \
  for (i = 0; i < n; i++)                                                 \
    vd[i] = vd[i] OP_2 vs2[i] OP_1 rs1;                                   \
                                                                          \
  return vs2[i - 1];                                                      \
}
#define DEF_VX_TERNARY_CASE_0_WRAP(T, OP_1, OP_2, NAME) \
  DEF_VX_TERNARY_CASE_0(T, OP_1, OP_2, NAME)
#define RUN_VX_TERNARY_CASE_0(T, NAME, vd, vs2, rs1, n) \
  test_vx_ternary_##NAME##_##T##_case_0 (vd, vs2, rs1, n)
#define RUN_VX_TERNARY_CASE_0_WRAP(T, NAME, vd, vs2, rs1, n) \
  RUN_VX_TERNARY_CASE_0(T, NAME, vd, vs2, rs1, n)

#define DEF_VX_TERNARY_CASE_1(T, OP_1, OP_2, NAME)                        \
void                                                                      \
test_vx_ternary_##NAME##_##T##_case_1 (T * restrict vd, T * restrict vs2, \
                                       T rs1, unsigned n)                 \
{                                                                         \
  for (unsigned i = 0; i < n; i++)                                        \
    vd[i] = vs2[i] OP_2 rs1 OP_1 vd[i];                                   \
}
#define DEF_VX_TERNARY_CASE_1_WRAP(T, OP_1, OP_2, NAME) \
  DEF_VX_TERNARY_CASE_1(T, OP_1, OP_2, NAME)
#define RUN_VX_TERNARY_CASE_1(T, NAME, vd, vs2, rs1, n) \
  test_vx_ternary_##NAME##_##T##_case_1 (vd, vs2, rs1, n)
#define RUN_VX_TERNARY_CASE_1_WRAP(T, NAME, vd, vs2, rs1, n) \
  RUN_VX_TERNARY_CASE_1(T, NAME, vd, vs2, rs1, n)

#define TEST_TERNARY_VX_SIGNED_0(T)                                \
  DEF_VX_TERNARY_CASE_0_WRAP(T, *, +, macc)                        \
  DEF_VX_TERNARY_CASE_1_WRAP(T, *, +, madd)                        \
  DEF_VX_TERNARY_CASE_0_WRAP(T, *, -, nmsac)                       \
  DEF_VX_TERNARY_CASE_1_WRAP(T, *, -, nmsub)                       \

#define TEST_TERNARY_VX_UNSIGNED_0(T)                              \
  DEF_VX_TERNARY_CASE_0_WRAP(T, *, +, macc)                        \
  DEF_VX_TERNARY_CASE_1_WRAP(T, *, +, madd)                        \
  DEF_VX_TERNARY_CASE_0_WRAP(T, *, -, nmsac)                       \
  DEF_VX_TERNARY_CASE_1_WRAP(T, *, -, nmsub)                       \

#endif
