#ifndef HAVE_DEFINED_VX_VF_BINARY_H
#define HAVE_DEFINED_VX_VF_BINARY_H

#include <stdint.h>

#undef HAS_INT128

#if __riscv_xlen == 64
#define HAS_INT128
typedef unsigned __int128 uint128_t;
typedef signed __int128 int128_t;
#endif

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

#define DEF_SAT_U_SUB(T)           \
T                                  \
test_##T##_sat_sub (T a, T b)      \
{                                  \
  return (a - b) & (-(T)(a >= b)); \
}

DEF_SAT_U_SUB(uint8_t)
DEF_SAT_U_SUB(uint16_t)
DEF_SAT_U_SUB(uint32_t)
DEF_SAT_U_SUB(uint64_t)

#define DEF_SAT_S_ADD(T, UT, MIN, MAX) \
T                                      \
test_##T##_sat_add (T x, T y)          \
{                                      \
  T sum = (UT)x + (UT)y;               \
  return (x ^ y) < 0                   \
    ? sum                              \
    : (sum ^ x) >= 0                   \
      ? sum                            \
      : x < 0 ? MIN : MAX;             \
}

DEF_SAT_S_ADD(int8_t, uint8_t, INT8_MIN, INT8_MAX)
DEF_SAT_S_ADD(int16_t, uint16_t, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD(int32_t, uint32_t, INT32_MIN, INT32_MAX)
DEF_SAT_S_ADD(int64_t, uint64_t, INT64_MIN, INT64_MAX)

#define DEF_SAT_S_SUB(T, UT, MIN, MAX) \
T                                      \
test_##T##_sat_sub (T x, T y)          \
{                                      \
  T minus = (UT)x - (UT)y;             \
  return (x ^ y) >= 0                  \
    ? minus                            \
    : (minus ^ x) >= 0                 \
      ? minus                          \
      : x < 0 ? MIN : MAX;             \
}

DEF_SAT_S_SUB(int8_t, uint8_t, INT8_MIN, INT8_MAX)
DEF_SAT_S_SUB(int16_t, uint16_t, INT16_MIN, INT16_MAX)
DEF_SAT_S_SUB(int32_t, uint32_t, INT32_MIN, INT32_MAX)
DEF_SAT_S_SUB(int64_t, uint64_t, INT64_MIN, INT64_MAX)

#define SAT_U_ADD_FUNC(T) test_##T##_sat_add
#define SAT_U_ADD_FUNC_WRAP(T) SAT_U_ADD_FUNC(T)

#define SAT_U_SUB_FUNC(T) test_##T##_sat_sub
#define SAT_U_SUB_FUNC_WRAP(T) SAT_U_SUB_FUNC(T)

#define SAT_S_ADD_FUNC(T) test_##T##_sat_add
#define SAT_S_ADD_FUNC_WRAP(T) SAT_S_ADD_FUNC(T)

#define SAT_S_SUB_FUNC(T) test_##T##_sat_sub
#define SAT_S_SUB_FUNC_WRAP(T) SAT_S_SUB_FUNC(T)

#define DEF_AVG_FLOOR(NT, WT)        \
NT                                   \
test_##NT##_avg_floor(NT x, NT y)    \
{                                    \
  return (NT)(((WT)x + (WT)y) >> 1); \
}

DEF_AVG_FLOOR(uint8_t, uint16_t)
DEF_AVG_FLOOR(uint16_t, uint32_t)
DEF_AVG_FLOOR(uint32_t, uint64_t)

DEF_AVG_FLOOR(int8_t, int16_t)
DEF_AVG_FLOOR(int16_t, int32_t)
DEF_AVG_FLOOR(int32_t, int64_t)

#define DEF_AVG_CEIL(NT, WT)             \
NT                                       \
test_##NT##_avg_ceil(NT x, NT y)         \
{                                        \
  return (NT)(((WT)x + (WT)y + 1) >> 1); \
}

DEF_AVG_CEIL(uint8_t, uint16_t)
DEF_AVG_CEIL(uint16_t, uint32_t)
DEF_AVG_CEIL(uint32_t, uint64_t)

DEF_AVG_CEIL(int8_t, int16_t)
DEF_AVG_CEIL(int16_t, int32_t)
DEF_AVG_CEIL(int32_t, int64_t)

#ifdef HAS_INT128
  DEF_AVG_FLOOR(uint64_t, uint128_t)
  DEF_AVG_FLOOR(int64_t, int128_t)

  DEF_AVG_CEIL(uint64_t, uint128_t)
  DEF_AVG_CEIL(int64_t, int128_t)
#endif

#define AVG_FLOOR_FUNC(T)      test_##T##_avg_floor
#define AVG_FLOOR_FUNC_WRAP(T) AVG_FLOOR_FUNC(T)

#define AVG_CEIL_FUNC(T)      test_##T##_avg_ceil
#define AVG_CEIL_FUNC_WRAP(T) AVG_CEIL_FUNC(T)

#define TEST_BINARY_VX_SIGNED_0(T)                                \
  DEF_VX_BINARY_CASE_0_WRAP(T, +, add)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)                            \
  DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)                   \
  DEF_VX_BINARY_CASE_0_WRAP(T, &, and)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, |, or)                             \
  DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, *, mul)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, /, div)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, %, rem)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, ==, eq)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, !=, ne)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, <, lt)                             \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_0_WARP(T), max)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_1_WARP(T), max)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_0_WARP(T), min)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_1_WARP(T), min)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, SAT_S_ADD_FUNC(T), sat_add)        \
  DEF_VX_BINARY_CASE_2_WRAP(T, SAT_S_SUB_FUNC(T), sat_sub)        \
  DEF_VX_BINARY_CASE_2_WRAP(T, AVG_FLOOR_FUNC_WRAP(T), avg_floor) \
  DEF_VX_BINARY_CASE_2_WRAP(T, AVG_CEIL_FUNC_WRAP(T), avg_ceil)   \

#define TEST_BINARY_VX_UNSIGNED_0(T)                              \
  DEF_VX_BINARY_CASE_0_WRAP(T, +, add)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)                            \
  DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)                   \
  DEF_VX_BINARY_CASE_0_WRAP(T, &, and)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, |, or)                             \
  DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, /, div)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, %, rem)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, ==, eq)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, !=, ne)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, <, ltu)                            \
  DEF_VX_BINARY_CASE_0_WRAP(T, <=, leu)                           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_0_WARP(T), max)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_1_WARP(T), max)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_0_WARP(T), min)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, MIN_FUNC_1_WARP(T), min)           \
  DEF_VX_BINARY_CASE_2_WRAP(T, SAT_U_ADD_FUNC(T), sat_add)        \
  DEF_VX_BINARY_CASE_2_WRAP(T, SAT_U_SUB_FUNC(T), sat_sub)        \
  DEF_VX_BINARY_CASE_2_WRAP(T, AVG_FLOOR_FUNC_WRAP(T), avg_floor) \
  DEF_VX_BINARY_CASE_2_WRAP(T, AVG_CEIL_FUNC_WRAP(T), avg_ceil)   \

/* For some special cases cannot be normalized as above  */

#define DEF_VX_MERGE_0(T)                                      \
void                                                           \
test_vx_merge_##T##_case_0 (T * restrict out, T * restrict in, \
                            T x,  unsigned n)                  \
{                                                              \
  for (unsigned i = 0; i < n; i++)                             \
    {                                                          \
      if (i % 2 == 0)                                          \
        out[i] = x;                                            \
      else                                                     \
        out[i] = in[i];                                        \
    }                                                          \
}

#define DEF_VX_MERGE_0_WRAP(T) DEF_VX_MERGE_0(T)

#define RUN_VX_MERGE_0(T, out, in, x, n) \
  test_vx_merge_##T##_case_0(out, in, x, n)
#define RUN_VX_MERGE_0_WRAP(T, out, in, x, n) RUN_VX_MERGE_0(T, out, in, x, n)

#endif
