/* { dg-do compile } */
/* { dg-options "-fgimple -O -fdump-tree-fre1" } */

__GIMPLE (ssa, startwith("fre")) char foo(char *p)
{
  char _1;

__BB(2):
  __MEM <char[4]> (p) = _Literal (char[4]) {};
  _1 = __MEM <char> (p + 1);
  return _1;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
