/* { dg-do compile } */
/* { dg-options "-fgimple -O -fdump-tree-fre1-details" } */

__GIMPLE (ssa, startwith("fre")) char foo(char *p)
{
  char _1;

__BB(2):
  __MEM <int> (p) = 0;
  _1 = __MEM <char> (p + 1);
  return _1;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
