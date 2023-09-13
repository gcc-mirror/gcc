/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/101590 */

int f_and_le(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len & -N;
  return newlen <= len; // return 1
}
int f_or_ge(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len | -N;
  return newlen >= len; // return 1
}

/* { dg-final { scan-tree-dump-not " <= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " >= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " & "  "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" } } */
