/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fno-tree-ch -ftree-loop-linear" } */

extern void abort ();

int
test (int n, int *a)
{
  int i, j;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      a[j] = i + n;

  if (a[0] != 31 || i + n - 1 != 31)
    abort ();

  return 0;
}

int main (void)
{
  int a[16];
  test (16, a);
  return 0;
}
