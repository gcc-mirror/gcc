#ifndef HAVE_DEFINED_VF_MULOP_H
#define HAVE_DEFINED_VF_MULOP_H

#include <stdint.h>

#define DEF_VF_MULOP_CASE_0(T, OP, NAME)                                       \
  void test_vf_mulop_##NAME##_##T##_case_0(T *restrict out, T *restrict in,    \
                                           T x, unsigned n) {                  \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = in[i] OP out[i] * x;                                            \
  }
#define DEF_VF_MULOP_CASE_0_WRAP(T, OP, NAME) DEF_VF_MULOP_CASE_0(T, OP, NAME)
#define RUN_VF_MULOP_CASE_0(T, NAME, out, in, x, n)                            \
  test_vf_mulop_##NAME##_##T##_case_0(out, in, x, n)
#define RUN_VF_MULOP_CASE_0_WRAP(T, NAME, out, in, x, n)                       \
  RUN_VF_MULOP_CASE_0(T, NAME, out, in, x, n)

#define VF_MULOP_BODY(op)                                                      \
  out[k + 0] = in[k + 0] op tmp * out[k + 0];                                  \
  out[k + 1] = in[k + 1] op tmp * out[k + 1];                                  \
  k += 2;

#define VF_MULOP_BODY_X4(op)                                                   \
  VF_MULOP_BODY(op)                                                            \
  VF_MULOP_BODY(op)

#define VF_MULOP_BODY_X8(op)                                                   \
  VF_MULOP_BODY_X4(op)                                                         \
  VF_MULOP_BODY_X4(op)

#define VF_MULOP_BODY_X16(op)                                                  \
  VF_MULOP_BODY_X8(op)                                                         \
  VF_MULOP_BODY_X8(op)

#define VF_MULOP_BODY_X32(op)                                                  \
  VF_MULOP_BODY_X16(op)                                                        \
  VF_MULOP_BODY_X16(op)

#define VF_MULOP_BODY_X64(op)                                                  \
  VF_MULOP_BODY_X32(op)                                                        \
  VF_MULOP_BODY_X32(op)

#define VF_MULOP_BODY_X128(op)                                                 \
  VF_MULOP_BODY_X64(op)                                                        \
  VF_MULOP_BODY_X64(op)

#define DEF_VF_MULOP_CASE_1(T, OP, NAME, BODY)                                       \
  void test_vf_mulop_##NAME##_##T##_case_1(T *restrict out, T *restrict in,    \
                                           T x, unsigned n) {                  \
    unsigned k = 0;                                                            \
    T tmp = x + 3;                                                             \
                                                                               \
    while (k < n) {                                                            \
      tmp = tmp * 0x3f;                                                        \
      BODY(OP)                                                                 \
    }                                                                          \
  }
#define DEF_VF_MULOP_CASE_1_WRAP(T, OP, NAME, BODY)                            \
  DEF_VF_MULOP_CASE_1(T, OP, NAME, BODY)

#endif
