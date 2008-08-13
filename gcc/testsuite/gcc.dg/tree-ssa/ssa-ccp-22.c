/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

/* Make sure we propagate through builtins.  */

int foo (unsigned b)
{
  unsigned t = -1;
  int x = b <= t;
  long l = __builtin_expect (x, 0);
  return l;
}

/* { dg-final { scan-tree-dump "return 1;" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
