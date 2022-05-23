/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim2" } */

int a[1024];

void foo (int x, int z)
{
  int i;
  int y = -x;
  for (i = 0; i < 1024; ++i)
    a[i] = x ? y : z;
}

void bar (int x, int z)
{
  int j;
  for (j = 0; j < 1024; ++j)
    {
      int i;
      int y = -j + z;
      for (i = 0; i < 1024; ++i)
	a[i] = x ? y : j;
    }
}

/* { dg-final { scan-tree-dump-times " = _\[0-9\]+ ? " 2 "lim2" } } */
