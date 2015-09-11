/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

/* Make sure CCP propagates through indirect calls.  */

int foo (void)
{
  int i = -5;
  int (*fn)(int) = __builtin_abs;
  int j = fn(i);
  return j + 5;
}

/* { dg-final { scan-tree-dump "return 10;" "ccp1" } } */
