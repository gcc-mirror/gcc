/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

struct A
{
  int a;
  int b;
};

foo (struct A *p, struct A *q)
{
  int x = p->a;
  if (p == q)
    return q->a;

  /* We should fold this to 'if (1)' but the assertion for 'p == q'
     was overwriting the assertion 'p != 0' from the first dereference
     of 'p'.  */
  if (p)
    return x + p->b;
}

/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
