#ifndef HAVE_DEFINED_VF_BINOP_H
#define HAVE_DEFINED_VF_BINOP_H

#include <stdint.h>

#define DEF_VF_BINOP_CASE_0(T, OP, NAME)                                       \
  void test_vf_binop_##NAME##_##T##_case_0 (T *restrict out, T *restrict in,   \
					    T f, unsigned n)                   \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = in[i] OP f;                                                     \
  }
#define DEF_VF_BINOP_CASE_0_WRAP(T, OP, NAME) DEF_VF_BINOP_CASE_0 (T, OP, NAME)
#define RUN_VF_BINOP_CASE_0(T, NAME, out, in, f, n)                            \
  test_vf_binop_##NAME##_##T##_case_0 (out, in, f, n)
#define RUN_VF_BINOP_CASE_0_WRAP(T, NAME, out, in, f, n)                       \
  RUN_VF_BINOP_CASE_0 (T, NAME, out, in, f, n)

#define DEF_VF_BINOP_REVERSE_CASE_0(T, OP, NAME)                               \
  void test_vf_binop_reverse_##NAME##_##T##_case_0 (T *restrict out,           \
						    T *restrict in, T f,       \
						    unsigned n)                \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = f OP in[i];                                                     \
  }
#define DEF_VF_BINOP_REVERSE_CASE_0_WRAP(T, OP, NAME)                          \
  DEF_VF_BINOP_REVERSE_CASE_0 (T, OP, NAME)
#define RUN_VF_BINOP_REVERSE_CASE_0(T, NAME, out, in, f, n)                    \
  test_vf_binop_reverse_##NAME##_##T##_case_0 (out, in, f, n)
#define RUN_VF_BINOP_REVERSE_CASE_0_WRAP(T, NAME, out, in, f, n)               \
  RUN_VF_BINOP_REVERSE_CASE_0 (T, NAME, out, in, f, n)

#define DEF_VF_BINOP_WIDEN_CASE_0(T1, T2, OP, NAME)                            \
  void test_vf_binop_widen_##NAME##_##T1##_case_0 (T2 *restrict out,           \
						   T1 *restrict in, T1 f,      \
						   unsigned n)                 \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = (T2) in[i] OP (T2) f;                                           \
  }
#define DEF_VF_BINOP_WIDEN_CASE_0_WRAP(T1, T2, OP, NAME)                       \
  DEF_VF_BINOP_WIDEN_CASE_0 (T1, T2, OP, NAME)
#define RUN_VF_BINOP_WIDEN_CASE_0(T1, T2, NAME, out, in, f, n)                 \
  test_vf_binop_widen_##NAME##_##T1##_case_0 (out, in, f, n)
#define RUN_VF_BINOP_WIDEN_CASE_0_WRAP(T1, T2, NAME, out, in, f, n)            \
  RUN_VF_BINOP_WIDEN_CASE_0 (T1, T2, NAME, out, in, f, n)

#define VF_BINOP_BODY(op)                                                      \
  out[k + 0] = in[k + 0] op tmp;                                               \
  out[k + 1] = in[k + 1] op tmp;                                               \
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

#define VF_BINOP_REVERSE_BODY(op)                                              \
  out[k + 0] = tmp op in[k + 0];                                               \
  out[k + 1] = tmp op in[k + 1];                                               \
  k += 2;

#define VF_BINOP_REVERSE_BODY_X4(op)                                           \
  VF_BINOP_REVERSE_BODY (op)                                                   \
  VF_BINOP_REVERSE_BODY (op)

#define VF_BINOP_REVERSE_BODY_X8(op)                                           \
  VF_BINOP_REVERSE_BODY_X4 (op)                                                \
  VF_BINOP_REVERSE_BODY_X4 (op)

#define VF_BINOP_REVERSE_BODY_X16(op)                                          \
  VF_BINOP_REVERSE_BODY_X8 (op)                                                \
  VF_BINOP_REVERSE_BODY_X8 (op)

#define VF_BINOP_REVERSE_BODY_X32(op)                                          \
  VF_BINOP_REVERSE_BODY_X16 (op)                                               \
  VF_BINOP_REVERSE_BODY_X16 (op)

#define VF_BINOP_REVERSE_BODY_X64(op)                                          \
  VF_BINOP_REVERSE_BODY_X32 (op)                                               \
  VF_BINOP_REVERSE_BODY_X32 (op)

#define VF_BINOP_REVERSE_BODY_X128(op)                                         \
  VF_BINOP_REVERSE_BODY_X64 (op)                                               \
  VF_BINOP_REVERSE_BODY_X64 (op)

#define DEF_VF_BINOP_REVERSE_CASE_1(T, OP, NAME, BODY)                         \
  void test_vf_binop_reverse_##NAME##_##T##_case_1 (T *restrict out,           \
						    T *restrict in, T f,       \
						    unsigned n)                \
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
#define DEF_VF_BINOP_REVERSE_CASE_1_WRAP(T, OP, NAME, BODY)                    \
  DEF_VF_BINOP_REVERSE_CASE_1 (T, OP, NAME, BODY)

#define DEF_MIN_0(T)                                                           \
  static inline T test_##T##_min_0 (T a, T b) { return a > b ? b : a; }

#define DEF_MIN_1(T)                                                           \
  static inline T test_##T##_min_1 (T a, T b) { return a >= b ? b : a; }

#define DEF_MAX_0(T)                                                           \
  static inline T test_##T##_max_0 (T a, T b) { return a > b ? a : b; }

#define DEF_MAX_1(T)                                                           \
  static inline T test_##T##_max_1 (T a, T b) { return a >= b ? a : b; }

DEF_MIN_0 (_Float16)
DEF_MIN_0 (float)
DEF_MIN_0 (double)

DEF_MIN_1 (_Float16)
DEF_MIN_1 (float)
DEF_MIN_1 (double)

DEF_MAX_0 (_Float16)
DEF_MAX_0 (float)
DEF_MAX_0 (double)

DEF_MAX_1 (_Float16)
DEF_MAX_1 (float)
DEF_MAX_1 (double)

#define MIN_FUNC_0(T) test_##T##_min_0
#define MIN_FUNC_0_WRAP(T) MIN_FUNC_0 (T)

#define MIN_FUNC_1(T) test_##T##_min_1
#define MIN_FUNC_1_WRAP(T) MIN_FUNC_1 (T)

#define MAX_FUNC_0(T) test_##T##_max_0
#define MAX_FUNC_0_WRAP(T) MAX_FUNC_0 (T)

#define MAX_FUNC_1(T) test_##T##_max_1
#define MAX_FUNC_1_WRAP(T) MAX_FUNC_1 (T)

#define DEF_VF_BINOP_CASE_2(T, FUNC, NAME)                                     \
  void test_vf_binop_##NAME##_##FUNC##_##T##_case_2 (T *restrict out,          \
						     T *restrict in, T f,      \
						     unsigned n)               \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = FUNC (in[i], f);                                                \
  }
#define DEF_VF_BINOP_CASE_2_WRAP(T, FUNC, NAME)                                \
  DEF_VF_BINOP_CASE_2 (T, FUNC, NAME)
#define RUN_VF_BINOP_CASE_2(T, NAME, FUNC, out, in, f, n)                      \
  test_vf_binop_##NAME##_##FUNC##_##T##_case_2 (out, in, f, n)
#define RUN_VF_BINOP_CASE_2_WRAP(T, NAME, FUNC, out, in, f, n)                 \
  RUN_VF_BINOP_CASE_2 (T, NAME, FUNC, out, in, f, n)

#define DEF_VF_BINOP_CASE_3(T, FUNC, NAME, BODY)                               \
  void test_vf_binop_##NAME##_##FUNC##_##T##_case_3 (T *restrict out,          \
						     T *restrict in, T f,      \
						     unsigned n)               \
  {                                                                            \
    unsigned k = 0;                                                            \
    T tmp = f + 3;                                                             \
                                                                               \
    while (k < n)                                                              \
      {                                                                        \
	tmp = tmp * 0x7.ap3;                                                   \
	BODY (FUNC)                                                            \
      }                                                                        \
  }
#define DEF_VF_BINOP_CASE_3_WRAP(T, FUNC, NAME, BODY)                          \
  DEF_VF_BINOP_CASE_3 (T, FUNC, NAME, BODY)

#define VF_BINOP_FUNC_BODY(func)                                               \
  out[k + 0] = func (in[k + 0], tmp);                                          \
  out[k + 1] = func (in[k + 1], tmp);                                          \
  k += 2;

#define VF_BINOP_FUNC_BODY_X4(op)                                              \
  VF_BINOP_FUNC_BODY (op)                                                      \
  VF_BINOP_FUNC_BODY (op)

#define VF_BINOP_FUNC_BODY_X8(op)                                              \
  VF_BINOP_FUNC_BODY_X4 (op)                                                   \
  VF_BINOP_FUNC_BODY_X4 (op)

#define VF_BINOP_FUNC_BODY_X16(op)                                             \
  VF_BINOP_FUNC_BODY_X8 (op)                                                   \
  VF_BINOP_FUNC_BODY_X8 (op)

#define VF_BINOP_FUNC_BODY_X32(op)                                             \
  VF_BINOP_FUNC_BODY_X16 (op)                                                  \
  VF_BINOP_FUNC_BODY_X16 (op)

#define VF_BINOP_FUNC_BODY_X64(op)                                             \
  VF_BINOP_FUNC_BODY_X32 (op)                                                  \
  VF_BINOP_FUNC_BODY_X32 (op)

#define VF_BINOP_FUNC_BODY_X128(op)                                            \
  VF_BINOP_FUNC_BODY_X64 (op)                                                  \
  VF_BINOP_FUNC_BODY_X64 (op)

#define DEF_VF_BINOP_WIDEN_CASE_1(TYPE1, TYPE2, OP, NAME)                      \
  void test_vf_binop_widen_##NAME##_##TYPE1##_##TYPE2##_case_1 (               \
    TYPE2 *__restrict dst, TYPE2 *__restrict dst2, TYPE2 *__restrict dst3,     \
    TYPE2 *__restrict dst4, TYPE1 *__restrict a, TYPE1 *__restrict b,          \
    TYPE1 *__restrict a2, TYPE1 *__restrict b2, int n)                         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] = (TYPE2) b[i] OP (TYPE2) * a;                                  \
	dst2[i] = (TYPE2) b[i] OP (TYPE2) * a2;                                \
	dst3[i] = (TYPE2) a[i] OP (TYPE2) * a2;                                \
	dst4[i] = (TYPE2) b2[i] OP (TYPE2) * a;                                \
      }                                                                        \
  }

#define DEF_VF_BINOP_WIDEN_CASE_2(T1, T2, OP, NAME)                            \
  void test_vf_binop_widen_##NAME##_##T1##_case_2 (T2 *restrict out,           \
						   T2 *restrict in, T1 f,      \
						   unsigned n)                 \
  {                                                                            \
    for (unsigned i = 0; i < n; i++)                                           \
      out[i] = in[i] OP (T2) f;                                                \
  }
#define DEF_VF_BINOP_WIDEN_CASE_2_WRAP(T1, T2, OP, NAME)                       \
  DEF_VF_BINOP_WIDEN_CASE_2 (T1, T2, OP, NAME)
#define RUN_VF_BINOP_WIDEN_CASE_2(T1, T2, NAME, out, in, f, n)                 \
  test_vf_binop_widen_##NAME##_##T1##_case_2 (out, in, f, n)
#define RUN_VF_BINOP_WIDEN_CASE_2_WRAP(T1, T2, NAME, out, in, f, n)            \
  RUN_VF_BINOP_WIDEN_CASE_2 (T1, T2, NAME, out, in, f, n)

#define DEF_VF_BINOP_WIDEN_CASE_3(TYPE1, TYPE2, OP, NAME)                      \
  void test_vf_binop_widen_##NAME##_##TYPE1##_##TYPE2##_case_3 (               \
    TYPE2 *__restrict dst, TYPE2 *__restrict dst2, TYPE2 *__restrict dst3,     \
    TYPE2 *__restrict dst4, TYPE1 *__restrict a, TYPE2 *__restrict b,          \
    TYPE1 *__restrict a2, TYPE2 *__restrict b2, int n)                         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] = b[i] OP (TYPE2) * a;                                          \
	dst2[i] = b[i] OP (TYPE2) * a2;                                        \
	dst3[i] = b2[i] OP (TYPE2) * a2;                                       \
	dst4[i] = b2[i] OP (TYPE2) * a;                                        \
      }                                                                        \
  }

#endif
