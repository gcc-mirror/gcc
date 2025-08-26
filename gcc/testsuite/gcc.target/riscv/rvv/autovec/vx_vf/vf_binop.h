#ifndef HAVE_DEFINED_VF_BINOP_H
#define HAVE_DEFINED_VF_BINOP_H

#include <stdint.h>

#define DEF_VF_BINOP_CASE_0(T, OP, NAME)                                       \
  void test_vf_binop_##NAME##_##T##_case_0 (T *restrict out, T *restrict in,   \
					    T f, unsigned n)                   \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = f OP in[i];                                                     \
  }
#define DEF_VF_BINOP_CASE_0_WRAP(T, OP, NAME) DEF_VF_BINOP_CASE_0 (T, OP, NAME)
#define RUN_VF_BINOP_CASE_0(T, NAME, out, in, f, n)                            \
  test_vf_binop_##NAME##_##T##_case_0 (out, in, f, n)
#define RUN_VF_BINOP_CASE_0_WRAP(T, NAME, out, in, f, n)                       \
  RUN_VF_BINOP_CASE_0 (T, NAME, out, in, f, n)

#define VF_BINOP_BODY(op)                                                      \
  out[k + 0] = tmp op in[k + 0];                                               \
  out[k + 1] = tmp op in[k + 1];                                               \
  k += 2;

#define VF_BINOP_BODY_X4(op)                                                   \
  VF_BINOP_BODY (op)                                                           \
  VF_BINOP_BODY (op)

#define VF_BINOP_BODY_X8(op)                                                   \
  VF_BINOP_BODY_X4 (op)                                                        \
  VF_BINOP_BODY_X4 (op)

#define VF_BINOP_BODY_X16(op)                                                  \
  VF_BINOP_BODY_X8 (op)                                                        \
  VF_BINOP_BODY_X8 (op)

#define VF_BINOP_BODY_X32(op)                                                  \
  VF_BINOP_BODY_X16 (op)                                                       \
  VF_BINOP_BODY_X16 (op)

#define VF_BINOP_BODY_X64(op)                                                  \
  VF_BINOP_BODY_X32 (op)                                                       \
  VF_BINOP_BODY_X32 (op)

#define VF_BINOP_BODY_X128(op)                                                 \
  VF_BINOP_BODY_X64 (op)                                                       \
  VF_BINOP_BODY_X64 (op)

#define DEF_VF_BINOP_CASE_1(T, OP, NAME, BODY)                                 \
  void test_vf_binop_##NAME##_##T##_case_1 (T *restrict out, T *restrict in,   \
					    T f, unsigned n)                   \
  {                                                                            \
    unsigned k = 0;                                                            \
    T tmp = f + 3.45;                                                          \
                                                                               \
    while (k < n)                                                              \
      {                                                                        \
	tmp = tmp * 0x3.fp2;                                                   \
	BODY (OP)                                                              \
      }                                                                        \
  }
#define DEF_VF_BINOP_CASE_1_WRAP(T, OP, NAME, BODY)                            \
  DEF_VF_BINOP_CASE_1 (T, OP, NAME, BODY)

#endif
