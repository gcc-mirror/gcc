/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1" } */

int a[2];
void foo(int i, int k)
{
  a[0] = i;
  if (k)
    a[0] = a[i] + k;
  else
    a[0] = a[i] + 3;
  a[0] = 0;
}

/* Only the last store remains.  */
/* { dg-final { scan-tree-dump-times " = " 1 "dse1" } } */
