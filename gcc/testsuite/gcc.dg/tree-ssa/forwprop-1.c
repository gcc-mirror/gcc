/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" }  */

/* We should be able to optimize this to b->t[i] = 1 during
   early optimizations.  */

struct a
{
  int t[10];
};

void f(struct a * b, __SIZE_TYPE__ i)
{
  int *c = b->t;
  c[i] = 1;
}

/* { dg-final { scan-tree-dump-times "t\\\[i.*\\\].* = 1;" 1 "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
