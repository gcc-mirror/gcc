/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

unsigned int f(unsigned int x) {
  return 1 / x;
}

/* { dg-final { scan-tree-dump-not "1 / x_..D.;" "optimized" } } */
/* { dg-final { scan-tree-dump "x_..D. == 1;" "optimized" } } */
