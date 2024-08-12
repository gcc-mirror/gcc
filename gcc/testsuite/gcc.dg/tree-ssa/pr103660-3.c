/* PR tree-optimization/103660 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop4-raw" } */

#define funcs(OP,n)		\
int min_##n(int a, int b) {	\
  int t;			\
  int t1;			\
  int t2;			\
  t1 = (a < b) * a;		\
  t2 = (a >= b) * b;		\
  t = t1 OP t2;			\
  return t;			\
}				\
int f_##n(int a, int b, int c,	\
	 int d) {		\
  int t;			\
  int t1;			\
  int t2;			\
  t1 = (a < b) * c;		\
  t2 = (a >= b) * d;		\
  t = t1 OP t2;			\
  return t;			\
}

funcs(^, xor)
funcs(+, plus)

/* min_xor/min_plus should produce min<a,b> */
/* f_xor/f_plus should produce (a < b) ? c : d */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr, " "forwprop4" } } */
/* { dg-final { scan-tree-dump-not   "plus_expr, "    "forwprop4" } } */
/* { dg-final { scan-tree-dump-times "min_expr, "   2 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times "lt_expr, "    2 "forwprop4" } } */
/* { dg-final { scan-tree-dump-times "cond_expr, "  2 "forwprop4" } } */
