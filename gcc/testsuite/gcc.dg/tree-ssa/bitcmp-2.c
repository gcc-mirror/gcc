/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/101590 */

int f_and_gt(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len & -N;
  return newlen > len; // return 0
}
int f_or_lt(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len | -N;
  return newlen < len; // return 0
}

/* { dg-final { scan-tree-dump-not " > " "optimized" } } */
/* { dg-final { scan-tree-dump-not " < " "optimized" } } */
/* { dg-final { scan-tree-dump-not " & "  "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */