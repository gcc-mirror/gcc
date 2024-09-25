/* PR tree-optimization/103660 */
/* { dg-do compile } */
/* { dg-options "-O1 -fgimple -fdump-tree-forwprop4-raw" } */

#define funcs(OP,n)		\
__GIMPLE()			\
int min_##n(int a, int b) {	\
  _Bool X;			\
  _Bool Y;			\
  int t;			\
  int t1;			\
  int t2;			\
  X = a < b;			\
  Y = a >= b;			\
  t1 = X ? a : 0;		\
  t2 = Y ? b : 0;		\
  t = t1 OP t2;			\
  return t;			\
}				\
__GIMPLE()			\
int f_##n(int a, int b, int c,	\
	 int d) {		\
  _Bool X;			\
  _Bool Y;			\
  int t;			\
  int t1;			\
  int t2;			\
  X = a < b;			\
  Y = a >= b;			\
  t1 = X ? c : 0;		\
  t2 = Y ? d : 0;		\
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
