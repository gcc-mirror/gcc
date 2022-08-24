/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(int x) {
  return 1 / x;
}

/* { dg-final { scan-tree-dump-not "1 / x_\[0-9]\+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump " <= 2;" "optimized" } } */
/* { dg-final { scan-tree-dump " \\? x_\[0-9]\+\\\(D\\\) : 0;" "optimized" } } */
