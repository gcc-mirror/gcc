#ifndef HAVE_SAT_ARITH
#define HAVE_SAT_ARITH

#include <stdint-gcc.h>

/******************************************************************************/
/* Saturation Add (unsigned and signed)                                       */
/******************************************************************************/
#define DEF_SAT_U_ADD_FMT_1(T)             \
T __attribute__((noinline))                \
sat_u_add_##T##_fmt_1 (T x, T y)           \
{                                          \
  return (x + y) | (-(T)((T)(x + y) < x)); \
}

#define DEF_SAT_U_ADD_FMT_2(T)           \
T __attribute__((noinline))              \
sat_u_add_##T##_fmt_2 (T x, T y)         \
{                                        \
  return (T)(x + y) >= x ? (x + y) : -1; \
}

#define DEF_SAT_U_ADD_FMT_3(T)                      \
T __attribute__((noinline))                         \
sat_u_add_##T##_fmt_3 (T x, T y)                    \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_add_overflow (x, y, &ret); \
  return (T)(-overflow) | ret;                      \
}

#define DEF_SAT_U_ADD_FMT_4(T)                           \
T __attribute__((noinline))                              \
sat_u_add_##T##_fmt_4 (T x, T y)                         \
{                                                        \
  T ret;                                                 \
  return __builtin_add_overflow (x, y, &ret) ? -1 : ret; \
}

#define DEF_SAT_U_ADD_FMT_5(T)                                \
T __attribute__((noinline))                                   \
sat_u_add_##T##_fmt_5 (T x, T y)                              \
{                                                             \
  T ret;                                                      \
  return __builtin_add_overflow (x, y, &ret) == 0 ? ret : -1; \
}

#define DEF_SAT_U_ADD_FMT_6(T)          \
T __attribute__((noinline))             \
sat_u_add_##T##_fmt_6 (T x, T y)        \
{                                       \
  return (T)(x + y) < x ? -1 : (x + y); \
}

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

#define RUN_SAT_U_ADD_FMT_1(T, x, y) sat_u_add_##T##_fmt_1(x, y)
#define RUN_SAT_U_ADD_FMT_2(T, x, y) sat_u_add_##T##_fmt_2(x, y)
#define RUN_SAT_U_ADD_FMT_3(T, x, y) sat_u_add_##T##_fmt_3(x, y)
#define RUN_SAT_U_ADD_FMT_4(T, x, y) sat_u_add_##T##_fmt_4(x, y)
#define RUN_SAT_U_ADD_FMT_5(T, x, y) sat_u_add_##T##_fmt_5(x, y)
#define RUN_SAT_U_ADD_FMT_6(T, x, y) sat_u_add_##T##_fmt_6(x, y)

#define RUN_VEC_SAT_U_ADD_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_1(out, op_1, op_2, N)

/******************************************************************************/
/* Saturation Sub (Unsigned and Signed)                                       */
/******************************************************************************/
#define DEF_SAT_U_SUB_FMT_1(T)     \
T __attribute__((noinline))        \
sat_u_sub_##T##_fmt_1 (T x, T y)   \
{                                  \
  return (x - y) & (-(T)(x >= y)); \
}

#define DEF_SAT_U_SUB_FMT_2(T)    \
T __attribute__((noinline))       \
sat_u_sub_##T##_fmt_2 (T x, T y)  \
{                                 \
  return (x - y) & (-(T)(x > y)); \
}

#define RUN_SAT_U_SUB_FMT_1(T, x, y) sat_u_sub_##T##_fmt_1(x, y)
#define RUN_SAT_U_SUB_FMT_2(T, x, y) sat_u_sub_##T##_fmt_2(x, y)

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

#define RUN_VEC_SAT_U_SUB_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_1(out, op_1, op_2, N)
#define RUN_VEC_SAT_U_SUB_FMT_2(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_2(out, op_1, op_2, N)

#endif
