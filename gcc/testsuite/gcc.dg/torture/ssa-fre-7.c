/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-fgimple -fdump-tree-fre1" } */

typedef int v4si __attribute__((vector_size(16)));

int __GIMPLE (ssa,startwith("fre"))
foo (int c)
{
  int * p;
  int i;
  int x[4];
  __SIZETYPE__ _1;
  __SIZETYPE__ _2;
  int _7;
  v4si _6;

  __BB(2):
  i_3 = 0;
  _1 = (long unsigned int) i_3;
  _2 = _1 * 4ul;
  p_4 = _Literal (int *) &x + _2;
  _6 = _Literal (v4si) { c_5(D), c_5(D), c_5(D), c_5(D) };
  __MEM <v4si> ((v4si *)p_4) = _6;
  _7 = x[0];
  return _7;
}

/* { dg-final { scan-tree-dump "return c_5\\(D\\);" "fre1" } } */
