#ifndef HAVE_SAT_ARITH
#define HAVE_SAT_ARITH

#include <stdint-gcc.h>

#define DEF_SAT_U_ADD_FMT_1(T)             \
T __attribute__((noinline))                \
sat_u_add_##T##_fmt_1 (T x, T y)           \
{                                          \
  return (x + y) | (-(T)((T)(x + y) < x)); \
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

#define RUN_VEC_SAT_U_ADD_FMT_1(T, out, op_1, op_2, N) \
  vec_sat_u_add_##T##_fmt_1(out, op_1, op_2, N)

#endif
