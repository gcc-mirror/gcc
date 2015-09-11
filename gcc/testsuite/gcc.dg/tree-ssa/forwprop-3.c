/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */

struct bar {
  int a[2];
};

int foo(struct bar *x)
{
  int *p = &x->a[0];
  int *q = &x->a[1];
  if (p < q)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump "Replaced .p_. < q_.. with .1." "forwprop1" } } */
