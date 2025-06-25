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

#define DEF_MAX_0(T)        \
static inline T             \
test_##T##_max_0 (T a, T b) \
{                           \
  return a > b ? a : b;     \
}

#define DEF_MAX_1(T)        \
static inline T             \
test_##T##_max_1 (T a, T b) \
{                           \
  return a >= b ? a : b;    \
}

DEF_MAX_0(int8_t)
DEF_MAX_0(int16_t)
DEF_MAX_0(int32_t)
DEF_MAX_0(int64_t)

DEF_MAX_1(int8_t)
DEF_MAX_1(int16_t)
DEF_MAX_1(int32_t)
DEF_MAX_1(int64_t)

DEF_MAX_0(uint8_t)
DEF_MAX_0(uint16_t)
DEF_MAX_0(uint32_t)
DEF_MAX_0(uint64_t)

DEF_MAX_1(uint8_t)
DEF_MAX_1(uint16_t)
DEF_MAX_1(uint32_t)
DEF_MAX_1(uint64_t)

#define MAX_FUNC_0(T) test_##T##_max_0
#define MAX_FUNC_0_WARP(T) MAX_FUNC_0(T)

#define MAX_FUNC_1(T) test_##T##_max_1
#define MAX_FUNC_1_WARP(T) MAX_FUNC_1(T)

#define DEF_MIN_0(T)        \
static inline T             \
test_##T##_min_0 (T a, T b) \
{                           \
  return a > b ? b : a;     \
}

#define DEF_MIN_1(T)        \
static inline T             \
test_##T##_min_1 (T a, T b) \
{                           \
  return a >= b ? b : a;    \
}

DEF_MIN_0(int8_t)
DEF_MIN_0(int16_t)
DEF_MIN_0(int32_t)
DEF_MIN_0(int64_t)

DEF_MIN_1(int8_t)
DEF_MIN_1(int16_t)
DEF_MIN_1(int32_t)
DEF_MIN_1(int64_t)

DEF_MIN_0(uint8_t)
DEF_MIN_0(uint16_t)
DEF_MIN_0(uint32_t)
DEF_MIN_0(uint64_t)

DEF_MIN_1(uint8_t)
DEF_MIN_1(uint16_t)
DEF_MIN_1(uint32_t)
DEF_MIN_1(uint64_t)

#define MIN_FUNC_0(T) test_##T##_min_0
#define MIN_FUNC_0_WARP(T) MIN_FUNC_0(T)

#define MIN_FUNC_1(T) test_##T##_min_1
#define MIN_FUNC_1_WARP(T) MIN_FUNC_1(T)

#define DEF_VX_BINARY_CASE_2(T, FUNC, NAME)                      \
void                                                             \
test_vx_binary_##NAME##_##FUNC##_##T##_case_2 (T * restrict out, \
					       T * restrict in,  \
					       T x, unsigned n)  \
{                                                                \
  for (unsigned i = 0; i < n; i++)                               \
    out[i] = FUNC (in[i], x);                                    \
}
#define DEF_VX_BINARY_CASE_2_WRAP(T, FUNC, NAME) \
  DEF_VX_BINARY_CASE_2(T, FUNC, NAME)
#define RUN_VX_BINARY_CASE_2(T, NAME, FUNC, out, in, x, n) \
  test_vx_binary_##NAME##_##FUNC##_##T##_case_2(out, in, x, n)
#define RUN_VX_BINARY_CASE_2_WRAP(T, NAME, FUNC, out, in, x, n) \
  RUN_VX_BINARY_CASE_2(T, NAME, FUNC, out, in, x, n)

#define DEF_VX_BINARY_CASE_3(T, FUNC, NAME, BODY)                \
void                                                             \
test_vx_binary_##NAME##_##FUNC##_##T##_case_3 (T * restrict out, \
					       T * restrict in,  \
					       T x, unsigned n)  \
{                                                                \
  unsigned k = 0;                                                \
  T tmp = x + 3;                                                 \
                                                                 \
  while (k < n)                                                  \
    {                                                            \
      tmp = tmp ^ 0x82;                                          \
      BODY(FUNC)                                                 \
    }                                                            \
}
#define DEF_VX_BINARY_CASE_3_WRAP(T, FUNC, NAME, BODY) \
  DEF_VX_BINARY_CASE_3(T, FUNC, NAME, BODY)

#define VX_BINARY_FUNC_BODY(func)     \
  out[k + 0] = func (in[k + 0], tmp); \
  out[k + 1] = func (in[k + 1], tmp); \
  k += 2;

#define VX_BINARY_FUNC_BODY_X4(op) \
  VX_BINARY_FUNC_BODY(op)          \
  VX_BINARY_FUNC_BODY(op)

#define VX_BINARY_FUNC_BODY_X8(op) \
  VX_BINARY_FUNC_BODY_X4(op)       \
  VX_BINARY_FUNC_BODY_X4(op)

#define VX_BINARY_FUNC_BODY_X16(op) \
  VX_BINARY_FUNC_BODY_X8(op)        \
  VX_BINARY_FUNC_BODY_X8(op)

#define VX_BINARY_FUNC_BODY_X32(op) \
  VX_BINARY_FUNC_BODY_X16(op)       \
  VX_BINARY_FUNC_BODY_X16(op)

#define VX_BINARY_FUNC_BODY_X64(op) \
  VX_BINARY_FUNC_BODY_X32(op)       \
  VX_BINARY_FUNC_BODY_X32(op)

#define VX_BINARY_FUNC_BODY_X128(op) \
  VX_BINARY_FUNC_BODY_X64(op)        \
  VX_BINARY_FUNC_BODY_X64(op)

#define DEF_SAT_U_ADD(T)                   \
T                                          \
test_##T##_sat_add (T a, T b)              \
{                                          \
  return (a + b) | (-(T)((T)(a + b) < a)); \
}

DEF_SAT_U_ADD(uint8_t)
DEF_SAT_U_ADD(uint16_t)
DEF_SAT_U_ADD(uint32_t)
DEF_SAT_U_ADD(uint64_t)

#define SAT_U_ADD_FUNC(T) test_##T##_sat_add
#define SAT_U_ADD_FUNC_WRAP(T) SAT_U_ADD_FUNC(T)

#define TEST_BINARY_VX_SIGNED_0(T)                      \
  DEF_VX_BINARY_CASE_0_WRAP(T, +, add)                  \
  DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)                  \
  DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)         \
  DEF_VX_BINARY_CASE_0_WRAP(T, &, and)                  \
  DEF_VX_BINARY_CASE_0_WRAP(T, |, or)                   \
  DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)                  \
  DEF_VX_BINARY_CASE_0_WRAP(T, *, mul)                  \
  DEF_VX_BINARY_CASE_0_WRAP(T, /, div)                  \
  DEF_VX_BINARY_CASE_0_WRAP(T, %, rem)                  \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_0_WARP(T), max) \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_1_WARP(T), max) \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_0_WARP(T), min) \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_1_WARP(T), min)

#define TEST_BINARY_VX_UNSIGNED_0(T)                       \
  DEF_VX_BINARY_CASE_0_WRAP(T, +, add)                     \
  DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)                     \
  DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)            \
  DEF_VX_BINARY_CASE_0_WRAP(T, &, and)                     \
  DEF_VX_BINARY_CASE_0_WRAP(T, |, or)                      \
  DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)                     \
  DEF_VX_BINARY_CASE_0_WRAP(T, /, div)                     \
  DEF_VX_BINARY_CASE_0_WRAP(T, %, rem)                     \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_0_WARP(T), max)    \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_1_WARP(T), max)    \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_0_WARP(T), min)    \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_1_WARP(T), min)    \
  DEF_VX_BINARY_CASE_2_WRAP(T, SAT_U_ADD_FUNC(T), sat_add)

#endif
