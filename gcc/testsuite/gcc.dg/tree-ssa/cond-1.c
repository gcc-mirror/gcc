/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int sub(int a, int b, int c, int d) {
  int e = (a == 0);
  int f = !e;
  c = b;
  d = b - a ;
  return ((-e & c) | (-f & d));
}

/* In the end we end up with `(a == 0) ? (b - a) : b`
   which then can be optimized to just `(b - a)`. */

/* { dg-final { scan-tree-dump-not "cond_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-not "eq_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-times "minus_expr," 1 "optimized" } } */
