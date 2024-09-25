/* PR tree-optimization/111150 */
/* { dg-do compile } */
/* { dg-options "-O1 -fgimple -fdump-tree-forwprop1-raw" } */

/* Checks if pattern (X ? e : f) == (Y ? e : f) gets optimized. */
__GIMPLE()
_Bool f1_(int a, int b, int c, int d, int e, int f) {
  _Bool X;
  _Bool Y;
  _Bool t;
  int t1;
  int t2;
  X = a == b;
  Y = c == d;
  /* Before the patch cond_expr was generated for these 2 statements. */
  t1 = X ? e : f;
  t2 = Y ? e : f;
  t = t1 == t2;
  return t;
}

/* Checks if pattern (X ? e : f) != (Y ? e : f) gets optimized. */
__GIMPLE()
_Bool f2_(int a, int b, int c, int d, int e, int f) {
  _Bool X;
  _Bool Y;
  _Bool t;
  int t1;
  int t2;
  X = a == b;
  Y = c == d;
  t1 = X ? e : f;
  t2 = Y ? e : f;
  t = t1 != t2;
  return t;
}

/* Checks if pattern (X ? e : f) == (Y ? f : e) gets optimized. */
__GIMPLE()
_Bool f3_(int a, int b, int c, int d, int e, int f) {
  _Bool X;
  _Bool Y;
  _Bool t;
  int t1;
  int t2;
  X = a == b;
  Y = c == d;
  t1 = X ? e : f;
  t2 = Y ? f : e;
  t = t1 == t2;
  return t;
}

/* Checks if pattern (X ? e : f) != (Y ? f : e) gets optimized. */
__GIMPLE()
_Bool f4_(int a, int b, int c, int d, int e, int f) {
  _Bool X;
  _Bool Y;
  _Bool t;
  int t1;
  int t2;
  X = a == b;
  Y = c == d;
  t1 = X ? e : f;
  t2 = Y ? f : e;
  t = t1 != t2;
  return t;
}

/* Should generate one bit_xor_expr for each testcase. */
/* { dg-final { scan-tree-dump-not "cond_expr, "  "forwprop1" } } */
/* 2 IOR, one each for f1 and f2.
   2 AND, one each for f3 and f4. */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 2  "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 2  "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 4 "forwprop1" } } */
/* 8 eq comparisons from each of `a == b`/`c == d`.
   2 more to check that `e == f`
   2 ne comparisons to check that `e != f`.   */
/* { dg-final { scan-tree-dump-times "<ne_expr, " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "<eq_expr, " 10 "forwprop1" } } */
