/* { dg-do run } */
/* { dg-options "-O3 -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define M (300)
#define N (200)

struct st
{
  double a[M];
  double b[M];
  double c[M][N];
};

int __attribute__ ((noinline))
foo (struct st *s)
{
  int i, j;
  for (i = 0; i != M;)
    {
      s->a[i] = 0.0;
      s->b[i] = 1.0;
      for (j = 0; 1; ++j)
	{
	  if (j == N) goto L2;
	  s->c[i][j] = 0.0;
	}
L2:
      ++i;
    }
  return 0;
}

struct st s;

int
main (void)
{
  return foo (&s);
}

/* { dg-final { scan-tree-dump "distributed: split to " "ldist" } } */
