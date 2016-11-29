/* { dg-do compile } */
/* { dg-options "-Ofast -fno-split-loops -fdump-tree-ifcvt-stats" } */
/* { dg-require-visibility "" } */

int b[256] = {0}, y;
void bar (int *);
int foo (int x, int n)
{
  int i;
  int a[128];

  for (i = 0; i < n; i++)
    {
      a[i] = i;
      if (x > i)
	b[i] = y;
    }
  bar (a);
  return 0;
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
