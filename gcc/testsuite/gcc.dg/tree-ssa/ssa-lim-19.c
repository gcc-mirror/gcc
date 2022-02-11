/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

volatile int x;
void
bar (int, char *, char *);
void
foo (int *a, int n, int m, int s, int t)
{
  int i;
  int j;
  int k;

  for (i = 0; i < m; i++) // Loop 1
    {
      if (__builtin_expect (x, 0))
	for (j = 0; j < n; j++) // Loop 2
	  for (k = 0; k < n; k++) // Loop 3
	    {
	      bar (s / 5, "one", "two");
	      a[t] = s;
	    }
      a[t] = t;
    }
}

/* { dg-final { scan-tree-dump-times "out of loop 2" 4 "lim2" } } */
/* { dg-final { scan-tree-dump-times "out of loop 1" 3 "lim2" } } */

