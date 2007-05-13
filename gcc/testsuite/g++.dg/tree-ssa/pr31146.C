/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop" } */

/* We should be able to optimize this to i[j] = 1 during
   early optimizations.  */

int i[5];
void foo (int j)
{
  void *p = &i[j];
  int *q = (int *)p;
  *q = 1;
}

/* { dg-final { scan-tree-dump "i\\\[j.*\\\] = 1;" "forwprop1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "i\\\[j.*\\\] = 1;" "forwprop2" } } */
/* { dg-final { cleanup-tree-dump "forwprop?" } } */
