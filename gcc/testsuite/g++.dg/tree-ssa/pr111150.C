/* PR tree-optimization/111150 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1 -Wno-psabi" } */

typedef int v4si __attribute((__vector_size__(4 * sizeof(int))));

/* Before the patch, VEC_COND_EXPR was generated for each statement in the
   function. This resulted in 3 VEC_COND_EXPR. */
v4si f1_(v4si a, v4si b, v4si c, v4si d, v4si e, v4si f) {
  v4si X = a == b ? e : f;
  v4si Y = c == d ? e : f;
  return (X != Y);
}

v4si f2_(v4si a, v4si b, v4si c, v4si d, v4si e, v4si f) {
  v4si X = a == b ? e : f;
  v4si Y = c == d ? e : f;
  return (X == Y);
}

v4si f3_(v4si a, v4si b, v4si c, v4si d, v4si e, v4si f) {
  v4si X = a == b ? e : f;
  v4si Y = c == d ? f : e;
  return (X != Y);
}

v4si f4_(v4si a, v4si b, v4si c, v4si d, v4si e, v4si f) {
  v4si X = a == b ? e : f;
  v4si Y = c == d ? f : e;
  return (X == Y);
}

/* For each testcase, should produce only one VEC_COND_EXPR for X^Y. */
/* { dg-final { scan-tree-dump-times " VEC_COND_EXPR " 4 "forwprop1" } } */
