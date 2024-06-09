/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/101590 */

/* These are the signed integer versions
   of `(a & b) CMP a` and `(a | b) CMP a`
   which can be optimized to 0. */

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

/* For `&`, the non-negativeness of b is not taken into account. */
int f_and_gt(int len) {
  len &= 0xfffff;
  const int N = 4;
  int newlen = len & -N;
  return newlen > len; // return 0
}
int f_and_gt_(int len, int N) {
  len &= 0xfffff;
  int newlen = len & -N;
  return newlen > len; // return 0
}

/* For `|`, to get a known value, b either needs to be non-negative
   or a constant.  For the negative constant case, we swap around the comparison. */
int f_or_lt_(int len, int N) {
  len &= 0xfffff;
  N &= 0xffff;
  int newlen = len | N;
  return newlen < len; // return 0
}
int f_or_ge(int len) {
  len &= 0xfffff;
  const int N = 4;
  int newlen = len | -N;
  return newlen >= len; // return 0
}

/* { dg-final { scan-tree-dump-not " > " "optimized" } } */
/* { dg-final { scan-tree-dump-not " < " "optimized" } } */
/* { dg-final { scan-tree-dump-not " & "  "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 4 "optimized" } } */
