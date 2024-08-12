/* PR tree-optimization/103660 */
/* Vector type version. */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-raw -Wno-psabi" } */

typedef int v4si __attribute((__vector_size__(4 * sizeof(int))));
#define funcs(OP,n)			\
v4si min_##n(v4si a, v4si b) {		\
  v4si X = -(a < b) * a;		\
  v4si Y = -(a >= b) * b;		\
  return (X OP Y);			\
}					\
v4si f_##n(v4si a, v4si b,		\
	   v4si c, v4si d) {		\
  v4si X = -(a < b) * c;		\
  v4si Y = -(a >= b) * d;		\
  return (X OP Y);			\
}


funcs(|, ior)

/* min_ior should produce min<a,b> or `a < b ? a : b` depending on if the target
   supports min on the vector type or not. */
/* f_ior should produce (a < b) ? c : d */
/* { dg-final { scan-tree-dump-not   "bit_ior_expr, "     "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "(?:lt_expr|min_expr), "        2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "(?:vec_cond_expr|min_expr), "  2 "forwprop1" } } */
