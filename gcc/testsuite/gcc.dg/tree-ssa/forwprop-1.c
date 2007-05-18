/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop" }  */

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

/* { dg-final { scan-tree-dump "t\\\[i.*\\\] = 1;" "forwprop1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "t\\\[i.*\\\] = 1;" "forwprop2" } } */
/* { dg-final { cleanup-tree-dump "forwprop?" } } */


