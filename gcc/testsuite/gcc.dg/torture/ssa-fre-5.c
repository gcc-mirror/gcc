/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-fgimple -fdump-tree-fre1" } */

typedef int v4si __attribute__((vector_size(16)));

int __GIMPLE (ssa,startwith("fre"))
foo ()
{
  int * p;
  int i;
  int x[4];
  long unsigned int _1;
  long unsigned int _2;
  int _7;

  __BB(2):
  i_3 = 0;
  _1 = (long unsigned int) i_3;
  _2 = _1 * 4ul;
  p_4 = _Literal (int *) &x + _2;
  __MEM <v4si> ((v4si *)p_4) = _Literal (v4si) { 1, 2, 3, 4 };
  _7 = x[0];
  return _7;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
