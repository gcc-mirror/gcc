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

#endif

#define DEF_MIN_0(T)                                                           \
  static inline T test_##T##_min_0 (T a, T b) { return a > b ? b : a; }

#define DEF_MIN_1(T)                                                           \
  static inline T test_##T##_min_1 (T a, T b) { return a >= b ? b : a; }

DEF_MIN_0 (_Float16)
DEF_MIN_0 (float)
DEF_MIN_0 (double)

DEF_MIN_1 (_Float16)
DEF_MIN_1 (float)
DEF_MIN_1 (double)

#define MIN_FUNC_0(T) test_##T##_min_0
#define MIN_FUNC_0_WRAP(T) MIN_FUNC_0 (T)

#define MIN_FUNC_1(T) test_##T##_min_1
#define MIN_FUNC_1_WRAP(T) MIN_FUNC_1 (T)

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
