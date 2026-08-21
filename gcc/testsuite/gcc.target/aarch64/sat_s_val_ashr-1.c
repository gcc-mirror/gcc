/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <stdint.h>

/* Write the saturation value of a signed overflow as
   (X >> (PREC - 1)) ^ MAX.  */

#define DEF(N, T, MX, SH)					\
  T f1_##N (T x, T y)						\
  {								\
    T s = (T) ((uint64_t) x + (uint64_t) y);			\
    return ((x ^ s) & ~(x ^ y)) < 0 ? ((x >> SH) ^ MX) : s;	\
  }								\
  T f2_##N (T x, T y)						\
  {								\
    T r;							\
    return __builtin_add_overflow (x, y, &r) ? ((x >> SH) ^ MX) : r; \
  }								\
  T f3_##N (T x, T y)						\
  {								\
    T r;							\
    return __builtin_sub_overflow (x, y, &r) ? ((x >> SH) ^ MX) : r; \
  }

DEF (32, int32_t, INT32_MAX, 31)
DEF (64, int64_t, INT64_MAX, 63)

/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 2 "optimized" } } */
