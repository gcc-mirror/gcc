/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/101590 */

int f_and_le(int len) {
  const int N = 4;
  int newlen = len & -N;
  return newlen <= len;
}
int f_or_ge(int len) {
  const int N = 4;
  int newlen = len | -N;
  return newlen >= len;
}

int f_and_gt(int len) {
  const int N = 4;
  int newlen = len & -N;
  return newlen > len;
}
int f_or_lt(int len) {
  const int N = 4;
  int newlen = len | -N;
  return newlen < len;
}

/* These cannot be optimized since we don't know if the sign
   can change or not. */
/* { dg-final { scan-tree-dump-times " > " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >= " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 2  "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\| " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 1;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 0;" "optimized" } } */
