/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

int foo(int *p, int b, float *q)
{
  int tem;
  if (b)
    {
      *q = 0;
      tem = *p;
    }
  else
    {
      *q = 1;
      tem = *p;
    }
  return *p - tem;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
