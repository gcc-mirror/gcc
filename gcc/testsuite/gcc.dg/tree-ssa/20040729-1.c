/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dce3" } */

foo ()
{
  volatile int *p;
  volatile int x;

  p = &x;
  *p = 3;
  return *p + 1;
}

/* The assignment to 'p' is dead and should be removed.  But the
   compiler was mistakenly thinking that the statement had volatile
   operands.  But 'p' itself is not volatile and taking the address of
   a volatile does not constitute a volatile operand.  */
/* { dg-final { scan-tree-dump-times "&x" 0 "dce3"} } */
/* { dg-final { cleanup-tree-dump "dce3" } } */
