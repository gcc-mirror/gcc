/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

/* We should be able to optimize this to i[j] = 1 during
   early optimizations.  */

int i[5];
void foo (int j)
{
  void *p = &i[j];
  int *q = (int *)p;
  *q = 1;
}

/* { dg-final { scan-tree-dump "MEM <int\\\[5\\\]> \\\[.*&i\\\]\\\[j.*\\\] =.* 1;" "forwprop1" } } */
