#ifndef HAVE_DEFINED_VF_MULOP_H
#define HAVE_DEFINED_VF_MULOP_H

#include <stdint.h>

#define DEF_VF_MULOP_CASE_0(T, OP, NEG, NAME)                                  \
  void test_vf_mulop_##NAME##_##T##_case_0 (T *restrict out, T *restrict in,   \
					    T f, unsigned n)                   \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = NEG (f * out[i] OP in[i]);                                      \
  }
#define DEF_VF_MULOP_CASE_0_WRAP(T, OP, NEG, NAME)                             \
  DEF_VF_MULOP_CASE_0 (T, OP, NEG, NAME)
#define RUN_VF_MULOP_CASE_0(T, NAME, out, in, x, n)                            \
  test_vf_mulop_##NAME##_##T##_case_0(out, in, x, n)
#define RUN_VF_MULOP_CASE_0_WRAP(T, NAME, out, in, x, n)                       \
  RUN_VF_MULOP_CASE_0(T, NAME, out, in, x, n)

#define VF_MULOP_BODY(op, neg)                                                 \
  out[k + 0] = neg (tmp * out[k + 0] op in[k + 0]);                            \
  out[k + 1] = neg (tmp * out[k + 1] op in[k + 1]);                            \
  k += 2;

#define VF_MULOP_BODY_X4(op, neg)                                              \
  VF_MULOP_BODY (op, neg)                                                      \
  VF_MULOP_BODY (op, neg)

#define VF_MULOP_BODY_X8(op, neg)                                              \
  VF_MULOP_BODY_X4 (op, neg)                                                   \
  VF_MULOP_BODY_X4 (op, neg)

#define VF_MULOP_BODY_X16(op, neg)                                             \
  VF_MULOP_BODY_X8 (op, neg)                                                   \
  VF_MULOP_BODY_X8 (op, neg)

#define VF_MULOP_BODY_X32(op, neg)                                             \
  VF_MULOP_BODY_X16 (op, neg)                                                  \
  VF_MULOP_BODY_X16 (op, neg)

#define VF_MULOP_BODY_X64(op, neg)                                             \
  VF_MULOP_BODY_X32 (op, neg)                                                  \
  VF_MULOP_BODY_X32 (op, neg)

#define VF_MULOP_BODY_X128(op, neg)                                            \
  VF_MULOP_BODY_X64 (op, neg)                                                  \
  VF_MULOP_BODY_X64 (op, neg)

#define DEF_VF_MULOP_CASE_1(T, OP, NEG, NAME, BODY)                            \
  void test_vf_mulop_##NAME##_##T##_case_1 (T *restrict out, T *restrict in,   \
					    T x, unsigned n)                   \
  {                                                                            \
    unsigned k = 0;                                                            \
    T tmp = x + 3;                                                             \
                                                                               \
    while (k < n)                                                              \
      {                                                                        \
	tmp = tmp * 0x3f;                                                      \
	BODY (OP, NEG)                                                         \
      }                                                                        \
  }
#define DEF_VF_MULOP_CASE_1_WRAP(T, OP, NEG, NAME, BODY)                       \
  DEF_VF_MULOP_CASE_1 (T, OP, NEG, NAME, BODY)

#endif
