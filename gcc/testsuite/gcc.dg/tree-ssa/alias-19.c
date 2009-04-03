/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-alias-vops" } */

const static int a;

int __attribute__((noinline))
foo(int i)
{
  const int *q;
  int b;
  if (i)
    q = &a;
  else
    q = &b;
  b = 1;
  /* We should not prune a from the points-to set of q.  */
  return *q;
}

extern void abort (void);
int main()
{
  if (foo(1) != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "q_. = { a b }" "alias" } } */
/* { dg-final { scan-tree-dump "q_., points-to vars: { a b }" "alias" } } */
/* { dg-final { cleanup-tree-dump "alias" } } */
