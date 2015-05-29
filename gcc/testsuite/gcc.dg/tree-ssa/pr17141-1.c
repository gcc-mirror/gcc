/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

struct A { int i; };
int
foo(struct A *locp, int str)
{
  int T355, *T356;
  T356 = &locp->i;
  *T356 = str;
  return locp->i;
}

/* We should have propagated &locp->i into its dereference.  */

/* { dg-final { scan-tree-dump "locp_\[^\\n\]* =" "forwprop1" } } */
