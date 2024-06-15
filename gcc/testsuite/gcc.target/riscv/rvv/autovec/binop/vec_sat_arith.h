#ifndef HAVE_VEC_SAT_ARITH
#define HAVE_VEC_SAT_ARITH

#include <stdint-gcc.h>

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

#define RUN_VEC_SAT_U_ADD_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_1(out, op_1, op_2, N)

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

#define RUN_VEC_SAT_U_SUB_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_1(out, op_1, op_2, N)
#define RUN_VEC_SAT_U_SUB_FMT_2(T, out, op_1, op_2, N) \
  vec_sat_u_sub_##T##_fmt_2(out, op_1, op_2, N)

#endif
