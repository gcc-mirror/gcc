/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/101590 */

/* These are the signed integer versions
   of `(a & b) CMP a` and `(a | b) CMP a`
   which can be optimized to 1. */

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

/* For `&`, the non-negativeness of b is not taken into account. */
int f_and_le(int len) {
  len &= 0xfffff;
  const int N = 4;
  int newlen = len & -N;
  return newlen <= len; // return 1
}
int f_and_le_(int len, int N) {
  len &= 0xfffff;
  int newlen = len & -N;
  return newlen <= len; // return 1
}


/* For `|`, to get a known value, b either needs to be non-negative
   or a constant.  For the negative constant case, we swap around the comparison. */
int f_or_ge_(int len, int N) {
  len &= 0xfffff;
  N &= 0xffff;
  int newlen = len | N;
  return newlen >= len; // return 1
}
int f_or_lt(int len) {
  len &= 0xfffff;
  const int N = 4;
  int newlen = len | -N;
  return newlen < len; // return 1
}

/* { dg-final { scan-tree-dump-not " <= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " >= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " & "  "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 4 "optimized" } } */
