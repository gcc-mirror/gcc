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

#define DEF_VF_MULOP_ACC_CASE_0(T, OP, NEG, NAME)                              \
  T test_vf_mulop_acc_##NAME##_##T##_case_0 (T *restrict out, T *restrict in,  \
					     T f, unsigned n)                  \
  {                                                                            \
    unsigned i;                                                                \
    for (i = 0; i < n; i++)                                                    \
      out[i] = NEG (f * in[i] OP out[i]);                                      \
    /* Ensure that we get acc rather than add by reusing the multiplicand. */  \
    return in[i - 1];                                                          \
  }
#define DEF_VF_MULOP_ACC_CASE_0_WRAP(T, OP, NEG, NAME)                         \
  DEF_VF_MULOP_ACC_CASE_0 (T, OP, NEG, NAME)
#define RUN_VF_MULOP_ACC_CASE_0(T, NAME, out, in, x, n)                        \
  test_vf_mulop_acc_##NAME##_##T##_case_0 (out, in, x, n)
#define RUN_VF_MULOP_ACC_CASE_0_WRAP(T, NAME, out, in, x, n)                   \
  RUN_VF_MULOP_ACC_CASE_0 (T, NAME, out, in, x, n)

#define DEF_VF_MULOP_WIDEN_CASE_0(T1, T2, OP, NEG, NAME)                       \
  void test_vf_mulop_widen_##NAME##_##T1##_case_0 (T2 *restrict out,           \
						   T1 *restrict in,            \
						   T1 *restrict f, unsigned n) \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = NEG ((T2) * f * (T2) in[i] OP out[i]);                          \
  }
#define DEF_VF_MULOP_WIDEN_CASE_0_WRAP(T1, T2, OP, NEG, NAME)                  \
  DEF_VF_MULOP_WIDEN_CASE_0 (T1, T2, OP, NEG, NAME)
#define RUN_VF_MULOP_WIDEN_CASE_0(T1, T2, NAME, out, in, x, n)                 \
  test_vf_mulop_widen_##NAME##_##T1##_case_0 (out, in, x, n)
#define RUN_VF_MULOP_WIDEN_CASE_0_WRAP(T1, T2, NAME, out, in, x, n)            \
  RUN_VF_MULOP_WIDEN_CASE_0 (T1, T2, NAME, out, in, x, n)

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

#define VF_MULOP_ACC_BODY(op, neg)                                             \
  out[k + 0] = neg (tmp * in[k + 0] op out[k + 1]);                            \
  out[k + 1] = neg (tmp * in[k + 1] op out[k + 1]);                            \
  k += 2;

#define VF_MULOP_ACC_BODY_X4(op, neg)                                          \
  VF_MULOP_ACC_BODY (op, neg)                                                  \
  VF_MULOP_ACC_BODY (op, neg)

#define VF_MULOP_ACC_BODY_X8(op, neg)                                          \
  VF_MULOP_ACC_BODY_X4 (op, neg)                                               \
  VF_MULOP_ACC_BODY_X4 (op, neg)

#define VF_MULOP_ACC_BODY_X16(op, neg)                                         \
  VF_MULOP_ACC_BODY_X8 (op, neg)                                               \
  VF_MULOP_ACC_BODY_X8 (op, neg)

#define VF_MULOP_ACC_BODY_X32(op, neg)                                         \
  VF_MULOP_ACC_BODY_X16 (op, neg)                                              \
  VF_MULOP_ACC_BODY_X16 (op, neg)

#define VF_MULOP_ACC_BODY_X64(op, neg)                                         \
  VF_MULOP_ACC_BODY_X32 (op, neg)                                              \
  VF_MULOP_ACC_BODY_X32 (op, neg)

#define VF_MULOP_ACC_BODY_X128(op, neg)                                        \
  VF_MULOP_ACC_BODY_X64 (op, neg)                                              \
  VF_MULOP_ACC_BODY_X64 (op, neg)

#define VF_MULOP_ACC_BODY_X256(op, neg)                                        \
  VF_MULOP_ACC_BODY_X128 (op, neg)                                             \
  VF_MULOP_ACC_BODY_X128 (op, neg)

#define DEF_VF_MULOP_ACC_CASE_1(T, OP, NEG, NAME, BODY)                        \
  void test_vf_mulop_acc_##NAME##_##T##_case_1 (T *restrict out,               \
						T *restrict in, T x,           \
						unsigned n)                    \
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
#define DEF_VF_MULOP_ACC_CASE_1_WRAP(T, OP, NEG, NAME, BODY)                   \
  DEF_VF_MULOP_ACC_CASE_1 (T, OP, NEG, NAME, BODY)

#define DEF_VF_MULOP_WIDEN_CASE_1(TYPE1, TYPE2, OP, NEG, NAME)                 \
  void test_vf_mulop_widen_##NAME##_##TYPE1##_##TYPE2##_case_1 (               \
    TYPE2 *__restrict dst, TYPE2 *__restrict dst2, TYPE2 *__restrict dst3,     \
    TYPE2 *__restrict dst4, TYPE1 *__restrict a, TYPE1 *__restrict b,          \
    TYPE1 *__restrict a2, TYPE1 *__restrict b2, int n)                         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] = NEG ((TYPE2) * a * (TYPE2) b[i] OP dst[i]);                   \
	dst2[i] = NEG ((TYPE2) * a2 * (TYPE2) b[i] OP dst2[i]);                \
	dst3[i] = NEG ((TYPE2) * a2 * (TYPE2) a[i] OP dst3[i]);                \
	dst4[i] = NEG ((TYPE2) * a * (TYPE2) b2[i] OP dst4[i]);                \
      }                                                                        \
  }

#endif
