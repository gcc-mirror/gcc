/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-dom2" } */

int __GIMPLE (ssa,startwith("dom"))
foo (void *p)
{
  int _2;

  __BB(2):
  __MEM <int> ((char *)p_1(D) + 4) = 20;
  _2 = __MEM <const int> ((int *)p_1(D) + 4);
  return _2;
}

/* { dg-final { scan-tree-dump "return 20;" "dom2" } } */
