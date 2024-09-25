/* PR tree-optimization/103660 */
/* { dg-do compile } */
/* { dg-options "-O1 -fgimple -fdump-tree-forwprop1-raw" } */

#define funcs(OP,n)		\
__GIMPLE()			\
int min_##n(int a, int b) {	\
  _Bool X;			\
  int t;			\
  int t1;			\
  int t2;			\
  X = a < b;			\
  t1 = X ? a : 0;		\
  t2 = X ? 0 : b;		\
  t = t1 OP t2;			\
  return t;			\
}				\
__GIMPLE()			\
int f_##n(int a, int b, int c,	\
	 int d) {		\
  _Bool X;			\
  int t;			\
  int t1;			\
  int t2;			\
  X = a < b;			\
  t1 = X ? c : 0;		\
  t2 = X ? 0 : d;		\
  t = t1 OP t2;			\
  return t;			\
}

funcs(|, ior)
funcs(^, xor)
funcs(+, plus)

/* min_i/min_ioror/min_plus should produce min<a,b> */
/* f_xor/f_ior/f_plus should produce (a < b) ? c : d */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr, " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not   "bit_ior_expr, " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not   "plus_expr, "    "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "min_expr, "   3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "lt_expr, "    3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "cond_expr, "  3 "forwprop1" } } */
