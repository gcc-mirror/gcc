/* { dg-do compile } */
/* { dg-options "-fgimple -O -fdump-tree-fre1" } */

__GIMPLE (ssa,startwith("fre")) char foo(char *p, int i)
{
  char _1;

__BB(2):
  __MEM <int> (p) = i_2(D);
  _1 = __MEM <char> (p + 1);
  return _1;
}

/* { dg-final { scan-tree-dump "BIT_FIELD_REF" "fre1" } } */
