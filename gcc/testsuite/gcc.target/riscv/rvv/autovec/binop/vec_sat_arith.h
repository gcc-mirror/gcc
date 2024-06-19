#ifndef HAVE_VEC_SAT_ARITH
#define HAVE_VEC_SAT_ARITH

#include <stdint-gcc.h>
#include <stdbool.h>

/******************************************************************************/
/* Saturation Add (unsigned and signed)                                       */
/******************************************************************************/
#define DEF_VEC_SAT_U_ADD_FMT_1(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_1 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = (x + y) | (-(T)((T)(x + y) < x));                     \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_2(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_2 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = (T)(x + y) >= x ? (x + y) : -1;                       \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_3(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_3 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      T overflow = __builtin_add_overflow (x, y, &ret);              \
      out[i] = (T)(-overflow) | ret;                                 \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_4(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_4 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      out[i] = __builtin_add_overflow (x, y, &ret) ? -1 : ret;       \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_5(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_5 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      out[i] = __builtin_add_overflow (x, y, &ret) == 0 ? ret : -1;  \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_6(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_6 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x <= (T)(x + y) ? (x + y) : -1;                       \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_7(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_7 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = (T)(x + y) < x ? -1 : (x + y);                        \
    }                                                                \
}

#define DEF_VEC_SAT_U_ADD_FMT_8(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_add_##T##_fmt_8 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x > (T)(x + y) ? -1 : (x + y);                        \
    }                                                                \
}

#define RUN_VEC_SAT_U_ADD_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_1(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_2(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_2(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_3(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_3(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_4(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_4(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_5(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_5(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_6(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_6(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_7(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_7(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_ADD_FMT_8(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_8(out, op_1, op_2, N)

/******************************************************************************/
/* Saturation Sub (Unsigned and Signed)                                       */
/******************************************************************************/
#define DEF_VEC_SAT_U_SUB_FMT_1(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_1 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = (x - y) & (-(T)(x >= y));                             \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_2(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_2 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = (x - y) & (-(T)(x > y));                              \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_3(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_3 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x > y ? x - y : 0;                                    \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_4(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_4 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x >= y ? x - y : 0;                                   \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_5(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_5 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x < y ? 0 : x - y;                                    \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_6(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_6 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      out[i] = x <= y ? 0 : x - y;                                   \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_7(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_7 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      T overflow = __builtin_sub_overflow (x, y, &ret);              \
      out[i] = ret & (T)(overflow - 1);                              \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_8(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_8 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      T overflow = __builtin_sub_overflow (x, y, &ret);              \
      out[i] = ret & (T)-(!overflow);                                \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_9(T)                                   \
void __attribute__((noinline))                                       \
vec_sat_u_sub_##T##_fmt_9 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                    \
  unsigned i;                                                        \
  for (i = 0; i < limit; i++)                                        \
    {                                                                \
      T x = op_1[i];                                                 \
      T y = op_2[i];                                                 \
      T ret;                                                         \
      bool overflow = __builtin_sub_overflow (x, y, &ret);           \
      out[i] = overflow ? 0 : ret;                                   \
    }                                                                \
}

#define DEF_VEC_SAT_U_SUB_FMT_10(T)                                   \
void __attribute__((noinline))                                        \
vec_sat_u_sub_##T##_fmt_10 (T *out, T *op_1, T *op_2, unsigned limit) \
{                                                                     \
  unsigned i;                                                         \
  for (i = 0; i < limit; i++)                                         \
    {                                                                 \
      T x = op_1[i];                                                  \
      T y = op_2[i];                                                  \
      T ret;                                                          \
      bool overflow = __builtin_sub_overflow (x, y, &ret);            \
      out[i] = !overflow ? ret : 0;                                   \
    }                                                                 \
}

#define RUN_VEC_SAT_U_SUB_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_1(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_2(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_2(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_3(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_3(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_4(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_4(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_5(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_5(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_6(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_6(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_7(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_7(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_8(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_8(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_9(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_9(out, op_1, op_2, N)

#define RUN_VEC_SAT_U_SUB_FMT_10(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_10(out, op_1, op_2, N)

#endif
