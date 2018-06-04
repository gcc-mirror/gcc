/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

void f(int n, char *p0, char *p1, char *p2, char *o)
{
  int t0, t1;
  __builtin_memcpy(&t0, p0, 1);
  __builtin_memcpy(&t1, p1, 1);
  if (n==3)
    __builtin_memcpy(o+2, p2, 1);
  __builtin_memcpy(o+0, &t0, 1);
  __builtin_memcpy(o+1, &t1, 1);
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1" } } */
