#ifndef HAVE_DEFINED_VX_VF_BINARY_H
#define HAVE_DEFINED_VX_VF_BINARY_H

#include <stdint.h>

#define DEF_VX_BINARY_CASE_0(T, OP, NAME)                                \
void                                                                     \
test_vx_binary_##NAME##_##T##_case_0 (T * restrict out, T * restrict in, \
                                      T x, unsigned n)                   \
{                                                                        \
  for (unsigned i = 0; i < n; i++)                                       \
    out[i] = in[i] OP x;                                                 \
}
#define DEF_VX_BINARY_CASE_0_WRAP(T, OP, NAME) \
  DEF_VX_BINARY_CASE_0(T, OP, NAME)
#define RUN_VX_BINARY_CASE_0(T, NAME, out, in, x, n) \
  test_vx_binary_##NAME##_##T##_case_0(out, in, x, n)
#define RUN_VX_BINARY_CASE_0_WRAP(T, NAME, out, in, x, n) \
  RUN_VX_BINARY_CASE_0(T, NAME, out, in, x, n)

#define DEF_VX_BINARY_REVERSE_CASE_0(T, OP, NAME)                   \
void                                                                \
test_vx_binary_reverse_##NAME##_##T##_case_0 (T * restrict out,     \
                                              T * restrict in, T x, \
                                              unsigned n)           \
{                                                                   \
  for (unsigned i = 0; i < n; i++)                                  \
    out[i] = x OP in[i];                                            \
}
#define DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, OP, NAME) \
  DEF_VX_BINARY_REVERSE_CASE_0(T, OP, NAME)
#define RUN_VX_BINARY_REVERSE_CASE_0(T, NAME, out, in, x, n) \
  test_vx_binary_reverse_##NAME##_##T##_case_0(out, in, x, n)
#define RUN_VX_BINARY_REVERSE_CASE_0_WRAP(T, NAME, out, in, x, n) \
  RUN_VX_BINARY_REVERSE_CASE_0(T, NAME, out, in, x, n)

#define VX_BINARY_BODY(op)       \
  out[k + 0] = in[k + 0] op tmp; \
  out[k + 1] = in[k + 1] op tmp; \
  k += 2;

#define VX_BINARY_BODY_X4(op) \
  VX_BINARY_BODY(op)          \
  VX_BINARY_BODY(op)

#define VX_BINARY_BODY_X8(op) \
  VX_BINARY_BODY_X4(op)       \
  VX_BINARY_BODY_X4(op)

#define VX_BINARY_BODY_X16(op) \
  VX_BINARY_BODY_X8(op)        \
  VX_BINARY_BODY_X8(op)

#define VX_BINARY_BODY_X32(op) \
  VX_BINARY_BODY_X16(op)       \
  VX_BINARY_BODY_X16(op)

#define VX_BINARY_BODY_X64(op) \
  VX_BINARY_BODY_X32(op)       \
  VX_BINARY_BODY_X32(op)

#define VX_BINARY_BODY_X128(op) \
  VX_BINARY_BODY_X64(op)        \
  VX_BINARY_BODY_X64(op)

#define DEF_VX_BINARY_CASE_1(T, OP, NAME, BODY)                          \
void                                                                     \
test_vx_binary_##NAME##_##T##_case_1 (T * restrict out, T * restrict in, \
                                      T x, unsigned n)                   \
{                                                                        \
  unsigned k = 0;                                                        \
  T tmp = x + 3;                                                         \
                                                                         \
  while (k < n)                                                          \
    {                                                                    \
      tmp = tmp ^ 0x3f;                                                  \
      BODY(OP)                                                           \
    }                                                                    \
}
#define DEF_VX_BINARY_CASE_1_WRAP(T, OP, NAME, BODY) \
  DEF_VX_BINARY_CASE_1(T, OP, NAME, BODY)

#define VX_BINARY_REVERSE_BODY(op) \
  out[k + 0] = tmp op in[k + 0];   \
  out[k + 1] = tmp op in[k + 1];   \
  k += 2;

#define VX_BINARY_REVERSE_BODY_X4(op) \
  VX_BINARY_REVERSE_BODY(op)          \
  VX_BINARY_REVERSE_BODY(op)

#define VX_BINARY_REVERSE_BODY_X8(op) \
  VX_BINARY_REVERSE_BODY_X4(op)       \
  VX_BINARY_REVERSE_BODY_X4(op)

#define VX_BINARY_REVERSE_BODY_X16(op) \
  VX_BINARY_REVERSE_BODY_X8(op)        \
  VX_BINARY_REVERSE_BODY_X8(op)

#define VX_BINARY_REVERSE_BODY_X32(op) \
  VX_BINARY_REVERSE_BODY_X16(op)       \
  VX_BINARY_REVERSE_BODY_X16(op)

#define VX_BINARY_REVERSE_BODY_X64(op) \
  VX_BINARY_REVERSE_BODY_X32(op)       \
  VX_BINARY_REVERSE_BODY_X32(op)

#define VX_BINARY_REVERSE_BODY_X128(op) \
  VX_BINARY_REVERSE_BODY_X64(op)        \
  VX_BINARY_REVERSE_BODY_X64(op)

#define DEF_VX_BINARY_REVERSE_CASE_1(T, OP, NAME, BODY)         \
void                                                            \
test_vx_binary_reverse_##NAME##_##T##_case_1 (T * restrict out, \
					      T * restrict in,  \
					      T x, unsigned n)  \
{                                                               \
  unsigned k = 0;                                               \
  T tmp = x + 3;                                                \
                                                                \
  while (k < n)                                                 \
    {                                                           \
      tmp = tmp ^ 0x3f;                                         \
      BODY(OP)                                                  \
    }                                                           \
}
#define DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, OP, NAME, BODY) \
  DEF_VX_BINARY_REVERSE_CASE_1(T, OP, NAME, BODY)

#endif
