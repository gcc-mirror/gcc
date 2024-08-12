/* PR tree-optimization/103660 */
/* Vector type version. */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-raw -Wno-psabi" } */

typedef int v4si __attribute((__vector_size__(4 * sizeof(int))));
#define funcs(OP,n)			\
v4si min_##n(v4si a, v4si b) {		\
  v4si X = a < b ? a : 0;		\
  v4si Y = a >= b ? b : 0;		\
  return (X OP Y);			\
}					\
v4si f_##n(v4si a, v4si b,		\
	   v4si c, v4si d) {		\
  v4si X = a < b ? c : 0;		\
  v4si Y = a >= b ? d : 0;		\
  return (X OP Y);			\
}


funcs(^, xor)
funcs(+, plus)

/* min_xor/min_plus should produce min<a,b> or `a < b ? a : b` depending on if the target
   supports min on the vector type or not. */
/* f_xor/f_plus should produce (a < b) ? c : d */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr, "     "forwprop1" } } */
/* { dg-final { scan-tree-dump-not   "plus_expr, "        "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "(?:lt_expr|min_expr), "        4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "(?:vec_cond_expr|min_expr), "  4 "forwprop1" } } */
