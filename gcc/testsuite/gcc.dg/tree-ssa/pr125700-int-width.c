/* PR tree-optimization/125700  */
/* Integer width variants to cover non-int widths and both signed/unsigned.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

#include <limits.h>

#define GEN_FUNCS(TAG, UTYPE, STYPE, UMAX, SMAX, SMIN) \
UTYPE \
fumin_##TAG (UTYPE x, UTYPE y) \
{ \
  if (x == (UMAX)) \
    return y; \
  return x < y ? x : y; \
} \
\
STYPE \
fsmin_##TAG (STYPE x, STYPE y) \
{ \
  if (x == (SMAX)) \
    return y; \
  return x < y ? x : y; \
} \
\
UTYPE \
fumax_##TAG (UTYPE x, UTYPE y) \
{ \
  if (x == 0) \
    return y; \
  return x > y ? x : y; \
} \
\
STYPE \
fsmax_##TAG (STYPE x, STYPE y) \
{ \
  if (x == (SMIN)) \
    return y; \
  return x > y ? x : y; \
}

GEN_FUNCS (char, unsigned char, signed char, UCHAR_MAX, SCHAR_MAX, SCHAR_MIN)
GEN_FUNCS (short, unsigned short, short, USHRT_MAX, SHRT_MAX, SHRT_MIN)
GEN_FUNCS (long, unsigned long, long, ULONG_MAX, LONG_MAX, LONG_MIN)
GEN_FUNCS (long_long, unsigned long long, long long, ULLONG_MAX, LLONG_MAX,
    LLONG_MIN)

/* { dg-final { scan-tree-dump-not {if \(} "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 8 "optimized" } } */
