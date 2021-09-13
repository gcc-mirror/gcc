/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1" } */

int a[2];
void foo(int i, int k, int j)
{
  a[0] = i;
  if (k)
    a[0] = a[i] + k;
  else
    {
      if (j)
        a[1] = 1;
      a[0] = a[i] + 3;
    }
  a[0] = 0;
}

/* The last stores to a[0] and a[1] remain.  */
/* { dg-final { scan-tree-dump-times " = " 2 "dse1" } } */
