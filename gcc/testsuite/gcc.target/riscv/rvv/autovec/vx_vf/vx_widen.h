#ifndef HAVE_DEFINED_VX_VF_BINARY_WIDEN_H
#define HAVE_DEFINED_VX_VF_BINARY_WIDEN_H

#include <stdint.h>

#undef HAS_INT128

#if __riscv_xlen == 64
#define HAS_INT128
typedef unsigned __int128 uint128_t;
typedef signed __int128 int128_t;
#endif

#define DEF_VX_WIDEN_BINARY_CASE_0(WT, NT, OP, NAME)                    \
void                                                                    \
test_vx_widen_binary_##NAME##_##WT##_##NT##_case_0 (WT * restrict vd,   \
						    NT * restrict vs2,  \
						    NT rs1, unsigned n) \
{                                                                       \
  for (unsigned i = 0; i < n; i++)                                      \
    vd[i] = (WT)vs2[i] OP (WT)rs1;                                      \
}

#define DEF_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, OP, NAME) \
  DEF_VX_WIDEN_BINARY_CASE_0(WT, NT, OP, NAME)
#define RUN_VX_WIDEN_BINARY_CASE_0(WT, NT, NAME, vd, vs2, rs1, n) \
  test_vx_widen_binary_##NAME##_##WT##_##NT##_case_0(vd, vs2, rs1, n)
#define RUN_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, NAME, vd, vs2, rs1, n) \
  RUN_VX_WIDEN_BINARY_CASE_0(WT, NT, NAME, vd, vs2, rs1, n)

#define DEF_VX_WIDEN_BINARY_CASE_1(WT, NT, OP, NAME)                    \
void                                                                    \
test_vx_widen_binary_##NAME##_##WT##_##NT##_case_1 (WT * restrict vd,   \
						    WT * restrict vs2,  \
						    NT rs1, unsigned n) \
{                                                                       \
  for (unsigned i = 0; i < n; i++)                                      \
    vd[i] = vs2[i] OP (WT)rs1;                                          \
}

#define DEF_VX_WIDEN_BINARY_CASE_1_WRAP(WT, NT, OP, NAME) \
  DEF_VX_WIDEN_BINARY_CASE_1(WT, NT, OP, NAME)
#define RUN_VX_WIDEN_BINARY_CASE_1(WT, NT, NAME, vd, vs2, rs1, n) \
  test_vx_widen_binary_##NAME##_##WT##_##NT##_case_1(vd, vs2, rs1, n)
#define RUN_VX_WIDEN_BINARY_CASE_1_WRAP(WT, NT, NAME, vd, vs2, rs1, n) \
  RUN_VX_WIDEN_BINARY_CASE_1(WT, NT, NAME, vd, vs2, rs1, n)

#define DEF_VX_WIDEN_TERNARY_CASE_0(WT, NT, OP1, OP2, NAME)             \
void                                                                    \
test_vx_widen_ternary_##NAME##_##WT##_##NT##_case_0 (WT * restrict vd,  \
						    NT * restrict vs2,  \
						    NT rs1,             \
						    unsigned n)         \
{                                                                       \
  for (unsigned i = 0; i < n; i++)                                      \
    vd[i] = vd[i] OP1 (WT)vs2[i] OP2 (WT)rs1;                           \
}

#define DEF_VX_WIDEN_TERNARY_CASE_0_WRAP(WT, NT, OP1, OP2, NAME) \
  DEF_VX_WIDEN_TERNARY_CASE_0(WT, NT, OP1, OP2, NAME)
#define RUN_VX_WIDEN_TERNARY_CASE_0(WT, NT, NAME, vd, vs2, rs1, n) \
  test_vx_widen_ternary_##NAME##_##WT##_##NT##_case_0(vd, vs2, rs1, n)
#define RUN_VX_WIDEN_TERNARY_CASE_0_WRAP(WT, NT, NAME, vd, vs2, rs1, n) \
  RUN_VX_WIDEN_TERNARY_CASE_0(WT, NT, NAME, vd, vs2, rs1, n)

#define TEST_WIDEN_BINARY_VX_UNSIGNED(WT, NT)     \
  DEF_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, +, add) \
  DEF_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, -, sub) \
  DEF_VX_WIDEN_BINARY_CASE_0_WRAP(WT, NT, *, mul) \
  DEF_VX_WIDEN_BINARY_CASE_1_WRAP(WT, NT, +, add) \
  DEF_VX_WIDEN_BINARY_CASE_1_WRAP(WT, NT, -, sub) \

#define TEST_WIDEN_TERNARY_VX_UNSIGNED(WT, NT)     \
  DEF_VX_WIDEN_TERNARY_CASE_0_WRAP(WT, NT, +, *, wmadd)

#endif
