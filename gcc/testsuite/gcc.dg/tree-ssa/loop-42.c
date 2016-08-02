/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivcanon-details" } */

void foo2 (unsigned int num, int *a)
{
  unsigned int i, n = (num - (num % 2));

  for(i = 0; i != n; i += 2)
    a[i] = 0;
}

void foo3 (unsigned int num, int *a)
{
  unsigned int i, n = (num - (num % 3));

  for(i = 0; i != n; i += 3)
    a[i] = 0;
}

void foo4 (unsigned int num, int *a)
{
  unsigned int i, n = (num - (num % 4));

  for(i = 0; i != n; i += 4)
    a[i] = 0;
}

void foo5 (unsigned int num, int *a)
{
  unsigned int i, n = (num - (num % 5));

  for(i = 0; i != n; i += 5)
    a[i] = 0;
}

/* { dg-final { scan-tree-dump-not "under assumptions " "ivcanon" } } */
