/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int f(int in) {
  in = in | 7;
  in = in ^ 3;
  in = (in & ~(unsigned long)1);
  return in;
}

/* { dg-final { scan-tree-dump-not "bit_and_expr, "  "optimized" } } */
