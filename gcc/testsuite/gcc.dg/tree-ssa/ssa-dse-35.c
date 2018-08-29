/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details" } */

int a[256];
void foo (void)
{
  a[0] = 1;
  for (int i = 5; i < 17; ++i)
    a[i] = i;
  a[0] = 2;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 1 "dse1" } } */
