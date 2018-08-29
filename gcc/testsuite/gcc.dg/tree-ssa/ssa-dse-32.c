/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

void f(int n)
{
  char *p = __builtin_malloc (1);
  int i;
  do
    *p = 0;
  while (++i < n);
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 1 "dse1" } } */
