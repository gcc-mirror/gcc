#ifndef HAVE_DEFINED_PR112600_5A_H
#define HAVE_DEFINED_PR112600_5A_H

#include <stdint.h>

#define DEF_SAT_ADD(T)   \
T sat_add_##T (T x, T y) \
{                        \
  T res;                 \
  res = x + y;           \
  res |= -(T)(res < x);  \
  return res;            \
}

#define VEC_DEF_SAT_ADD(T)                       \
void vec_sat_add(T * restrict a, T * restrict b) \
{                                                \
  for (int i = 0; i < 16; i++)                   \
    b[i] = sat_add_##T (a[i], b[i]);             \
}

#endif
