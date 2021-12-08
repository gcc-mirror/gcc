/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

volatile int x;
void
bar (int, char *, char *);
void
foo (int *a, int n, int k)
{
  int i;

  for (i = 0; i < n; i++)
    {
      if (__builtin_expect (x, 0))
	bar (k / 5, "one", "two");
      a[i] = k;
    }
}

/* { dg-final { scan-tree-dump-not "out of loop 1" "lim2" } } */

